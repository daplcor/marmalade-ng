(namespace "free")
(define-keyset "free.guardian-gov" (read-keyset "g"))

(module guardian-royalty GOVERNANCE
    (implements marmalade-ng.token-policy-ng-v1)
    (use marmalade-ng.token-policy-ng-v1 [token-info])
    (use marmalade-ng.util-policies)
    (use free.util-fungible [enforce-valid-account])
    (use free.util-strings [to-string])
    (use free.util-math [min])
  
    ;-----------------------------------------------------------------------------
    ; Governance
    ;-----------------------------------------------------------------------------
    (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.guardian-gov"))
    )
  
    ;-----------------------------------------------------------------------------
    ; Schemas and Tables
    ;-----------------------------------------------------------------------------
    ; Store the royalty informations per token
    (defschema royalty-token-sch
      token-id:string
      creator-account:string
      creator-guard:guard
      recipients:[object:{recipient-sch}] ; Uses the recipient schema to format multiple roylaty recipients
      currencies:[module{fungible-v2}]
    )
  
    (deftable royalty-tokens:{royalty-token-sch})
  
    ; Store the royalty information per recipient
    (defschema recipient-sch
        idx:string
        account:string
        guard:guard
        rate:decimal
    )
        
    ; Store the royalty informations per sale
    (defschema royalty-sale-sch
      currency:module{fungible-v2}
    )
  
    (deftable royalty-sales:{royalty-sale-sch})

    ;-----------------------------------------------------------------------------
    ; Constants
    ;-----------------------------------------------------------------------------
    (defconst MAX_RECIPIENTS 3)
    (defconst ROYALTY_RATE 0.05)

    (defconst RECIPIENT1 "r1")
    (defconst RECIPIENT2 "r2")
    (defconst RECIPIENT3 "r3")

    ;-----------------------------------------------------------------------------
    ; Capabilities and events
    ;-----------------------------------------------------------------------------
    (defcap ROYALTY-PAID (token-id:string recipient:string amount:decimal)
      @doc "Event emitted when a royalty is paid to a creator"
      @event
      true)
  
    (defcap UPDATE-ROYALTY (token-id:string)
      @doc "Capability to modify the royalty"
      (with-read royalty-tokens token-id {'creator-guard:=current-guard}
        (enforce-guard current-guard))
    )
  
    ;-----------------------------------------------------------------------------
    ; Input data
    ;-----------------------------------------------------------------------------
    (defschema royalty-init-msg-sch
      creator_acct:string 
      creator_guard:guard 
      recipients:[object:{recipient-sch}]
      currencies:[module{fungible-v2}] ; List of currencies allowed for royalty payment
    )
  
    (defun read-royalty-init-msg:object{royalty-init-msg-sch} (token:object{token-info})
      (enforce-get-msg-data "royalty" token))
  
    ;-----------------------------------------------------------------------------
    ; Util functions
    ;-----------------------------------------------------------------------------
    (defun enforce-valid-fungibles:bool (currencies:[module{fungible-v2}])
      (enforce (and? (< 0) (>= 20) (length currencies)) "Incorrect currencies list"))

    (defun enforce-valid-recipients:bool (recipients:[object:{recipient-sch}])
      (enforce (<= (length recipients) MAX_RECIPIENTS) "Must have 3 or less recipients")
      (let ((total-rate (fold (+) 0.0 (map (at "rate") recipients))))
        (enforce (= total-rate 1.0) "Total rate must equal 100%"))
      true
    )
  
    ;-----------------------------------------------------------------------------
    ; Policy hooks
    ;-----------------------------------------------------------------------------
    (defun rank:integer ()
      RANK-ROYALTY)
  
    (defun enforce-init:bool (token:object{token-info})
      (require-capability (marmalade-ng.ledger.POLICY-ENFORCE-INIT token guardian-royalty))
      (let* ((royalty-init-msg (read-royalty-init-msg token))
            (token-id (at 'id token))
            (recip (at 'recipients royalty-init-msg)))
        (bind royalty-init-msg {'creator_acct:=c-a, 'creator_guard:=c-g, 'recipients:=recipients, 'currencies:=cur}
          (enforce-valid-account c-a)
        ;    (enforce-valid-rate rate)
          (enforce-valid-recipients recip)
          (enforce-valid-fungibles cur)
          (insert royalty-tokens token-id {'token-id:token-id,
                                           'creator-account:c-a,
                                           'creator-guard:c-g,
                                           'recipients:recipients,
                                           'currencies:cur
                                           })))
      true
    )
  
    (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
      true)
  
    (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
      true)
  
    (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
      true)
  
    (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
      (require-capability (marmalade-ng.ledger.POLICY-ENFORCE-OFFER token (pact-id) guardian-royalty))
      ; Read the fungible currency from the sale message
      (bind (enforce-read-sale-msg token) {'currency:=currency}
        ; Fetch from the database the allowed currencies for this token
        (with-read royalty-tokens (at 'id token) {'currencies:=allowed-currencies}
          ; And finally check that the requested currency is allowed
          ; Because of https://github.com/kadena-io/pact/issues/1307
          ;   until this one will be fixed, we have to compare by stringified versions
          (enforce (contains (to-string currency) (map (to-string) allowed-currencies))
                   "Currency is not allowed"))
  
        ;Store the currency for this sale-id => This will be needed during (enforce-settle)
        (insert royalty-sales (pact-id) {'currency: currency}))
      false ; We always return false because the royalty policy does not handle a sale
    )
  
    (defun enforce-sale-withdraw:bool (token:object{token-info})
      true)
  
    (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
      true)
  
    (defun enforce-sale-settle:bool (token:object{token-info})
      (require-capability (marmalade-ng.ledger.POLICY-ENFORCE-SETTLE token (pact-id) guardian-royalty))
      (with-read royalty-sales (pact-id) {'currency:=currency:module{fungible-v2}}
        (with-read royalty-tokens (at 'id token) {'recipients:=recipients}
          (let* ((escrow (marmalade-ng.ledger.escrow))
                 (escrow-balance (currency::get-balance escrow))
                 (royalty-amount (floor (* escrow-balance ROYALTY_RATE) (currency::precision))))
            (install-capability (currency::TRANSFER escrow (get-WALLET-account) royalty-amount))
            (currency::transfer escrow (get-WALLET-account) royalty-amount)
            (with-capability (WALLET)
            (pay-royalties currency royalty-amount token recipients))
            false)))
    )

    (defun pay-royalties:bool 
      (currency:module{fungible-v2} 
       royalty-amount:decimal 
       token:object{token-info} 
       recipients:[object:{recipient-sch}])
      (require-capability (WALLET))
      (map (pay-single-royalty currency royalty-amount token) recipients)
      true
    )

    (defun pay-single-royalty:bool
      (currency:module{fungible-v2}
       royalty-amount:decimal
       token:object{token-info}
       recipient:object{recipient-sch})
      (require-capability (WALLET))
      (bind recipient {'account:=account, 'guard:=guard, 'rate:=rate}
        (let ((amount (floor (* royalty-amount rate) (currency::precision))))
          (install-capability (currency::TRANSFER (get-WALLET-account) account amount))
          (currency::transfer-create (get-WALLET-account) account guard amount)
          (emit-event (ROYALTY-PAID (at 'id token) account amount))
          true))
    )

    ;-----------------------------------------------------------------------------
    ; External callable admin functions
    ;-----------------------------------------------------------------------------
    (defun rotate:string (token-id:string creator-account:string creator-guard:guard)
      @doc "Change/rotate the creator-account/creator-guard of the given tokenID"
      (enforce-valid-account creator-account)
      (with-capability (UPDATE-ROYALTY token-id)
        (update royalty-tokens token-id {'creator-account:creator-account,
                                         'creator-guard:creator-guard}))
    )

    (defun update-recipients:string (token-id:string recipients:[object:{recipient-sch}])
      @doc "Update the recipients for a given token"
      (enforce-valid-recipients recipients)
      (with-capability (UPDATE-ROYALTY token-id)
        (update royalty-tokens token-id {'recipients:recipients}))
    )
  
    (defun update-allowed-currencies:string (token-id:string currencies:[module{fungible-v2}])
      @doc "Change the list of allowed currencies"
      (enforce-valid-fungibles currencies)
      (with-capability (UPDATE-ROYALTY token-id)
        (update royalty-tokens token-id {'currencies:currencies}))
    )
  
    ;-----------------------------------------------------------------------------
    ; View functions
    ;-----------------------------------------------------------------------------
    (defun get-royalty-details:object{royalty-token-sch} (token-id:string)
      @doc "Return the details of the royalty spec for a token-id"
       (read royalty-tokens token-id))

    (defun get-recipient-data:object (token-id:string c:string)
      (let* ((recipients (get-more token-id))
             (filtered-recipients (filter (lambda (r) (= (at 'idx r) c)) recipients)))
        (enforce (> (length filtered-recipients) 0) (format "Recipient {} not found" [c]))
        (at 0 filtered-recipients)))

    (defun get-more (token-id:string)
      (let* ((token (read royalty-tokens token-id))
            (recipients (at 'recipients token))
      )recipients))
  
    ;-----------------------------------------------------------------------------
    ; View functions (local only)
    ;-----------------------------------------------------------------------------
    (defun get-royalty-details-per-creator:[object{royalty-token-sch}] (creator:string)
      @doc "Return the details of the royalty specs of all tokens of a given creator"
      (select royalty-tokens (where 'creator-account (= creator))))

    ; #############################################
    ;                 Wallet Account
    ; #############################################


    (defcap WALLET ()
    @doc "Checks to make sure the guard for the given account name is satisfied"
    true
    )

    (defun require-WALLET ()
    @doc "The function used when building the user guard for managed accounts"
    (require-capability (WALLET))
    )

    (defun create-WALLET-guard ()
    @doc "Creates the user guard"
    (create-user-guard (require-WALLET))
    )

    (defun get-WALLET-account ()
    (create-principal (create-WALLET-guard))
    )

    (defun init ()
      (coin.create-account (get-WALLET-account) (create-WALLET-guard)))
)
  
  (init)