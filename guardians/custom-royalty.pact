(namespace "free")
(define-keyset "free.guardian-gov" (read-keyset "g"))

(module guardian-royalty GOVERNANCE
    (implements marmalade-ng.token-policy-ng-v1)
    (use marmalade-ng.token-policy-ng-v1 [token-info])
    (use marmalade-ng.util-policies)
    (use free.util-fungible [enforce-valid-account])
    (use free.util-strings [to-string])
  
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
      recipients:[object:{recipient-sch}]
      currencies:[module{fungible-v2}]
    )
  
    (deftable royalty-tokens:{royalty-token-sch})
  
    ; Store the royalty information per recipient
    (defschema recipient-sch
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
    (defconst MAX_RECIPIENTS 4)
    (defconst MAX_TOTAL_RATE 0.10)
  
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
      (enforce (<= (length recipients) MAX_RECIPIENTS) "Too many recipients")
      (enforce (> (length recipients) 0) "At least one recipient required")
      (let ((total-rate (fold (+) 0.0 (map (at "rate") recipients))))
        (enforce (<= total-rate MAX_TOTAL_RATE) "Total rate exceeds maximum allowed"))
      true
    )
  
    ;-----------------------------------------------------------------------------
    ; Policy hooks
    ;-----------------------------------------------------------------------------
    (defun rank:integer ()
      RANK-ROYALTY)
  
    (defun enforce-init:bool (token:object{token-info})
      (require-capability (marmalade-ng.ledger.POLICY-ENFORCE-INIT token guardian-royalty))
      (let ((royalty-init-msg (read-royalty-init-msg token))
            (token-id (at 'id token)))
        (bind royalty-init-msg {'creator_acct:=c-a, 'creator_guard:=c-g, 'recipients:=recipients, 'currencies:=cur}
          (enforce-valid-account c-a)
        ;    (enforce-valid-rate rate)
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
                (escrow-balance (currency::get-balance escrow)))
                (fold (pay-royalty escrow currency escrow-balance (at 'id token)) 0.0 recipients))))
    )

    (defun pay-royalty:decimal (escrow:string currency:module{fungible-v2} escrow-balance:decimal token-id:string prev-paid:decimal recipient:object{recipient-sch})
    (require-capability (marmalade-ng.ledger.POLICY-ENFORCE-SETTLE token-id (pact-id) guardian-royalty))
      (bind recipient {'account:=account, 'guard:=guard, 'rate:=rate}
        (let* ((royalty-amount (floor (* rate escrow-balance) (currency::precision)))
               (current-guard (try guard (at 'guard (currency::details account)))))
          (if (and (> royalty-amount 0.0) (= guard current-guard))
              (let ((total-paid (+ prev-paid royalty-amount)))
                (install-capability (currency::TRANSFER escrow account royalty-amount))
                (currency::transfer-create escrow account guard royalty-amount)
                (emit-event (ROYALTY-PAID token-id account royalty-amount))
                total-paid)
              prev-paid)))
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
  
    ;-----------------------------------------------------------------------------
    ; View functions (local only)
    ;-----------------------------------------------------------------------------
    (defun get-royalty-details-per-creator:[object{royalty-token-sch}] (creator:string)
      @doc "Return the details of the royalty specs of all tokens of a given creator"
      (select royalty-tokens (where 'creator-account (= creator))))
)
  
  