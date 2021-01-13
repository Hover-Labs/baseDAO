// SPDX-FileCopyrightText: 2020 TQ Tezos
// SPDX-License-Identifier: LicenseRef-MIT-TQ

// Corresponds to Proposal.hs module

#include "types.mligo"
#include "token/fa2.mligo"
#include "management.mligo"
#include "permit.mligo"

// -----------------------------------------------------------------
// Helper
// -----------------------------------------------------------------

[@inline]
let to_proposal_key (propose_params, sender_addr : propose_params * address): proposal_key =
  Crypto.blake2b (Bytes.pack (propose_params, sender_addr))

[@inline]
let check_if_proposal_exist (proposal_key, store : proposal_key * storage): proposal =
  match Map.find_opt proposal_key store.proposals with
    Some p -> p
  | None -> (failwith("PROPOSAL_NOT_EXIST") : proposal)

// TODO: make all checks return storage

[@inline]
let ensure_voting_period_is_not_over (proposal, store : proposal * storage): unit =
  if Tezos.now >= proposal.start_date + int(store.voting_period)
    then failwith("VOTING_PERIOD_OVER")
    else ()

[@inline]
let ensure_proposal_is_unique (propose_params, store : propose_params * storage): unit =
  let proposal_key = to_proposal_key(propose_params, sender) in
  if Map.mem proposal_key store.proposals
    then failwith("PROPOSAL_NOT_UNIQUE")
    else ()

// -----------------------------------------------------------------
// Propose
// -----------------------------------------------------------------

[@inline]
let check_is_proposal_valid (config, propose_params, store : config * propose_params * storage): unit =
  if config.proposal_check (propose_params, store)
    then ()
    else failwith("FAIL_PROPOSAL_CHECK")

// TODO: proper balance error
[@inline]
let check_proposer_unfrozen_token (propose_params, store : propose_params * storage): unit =
  let current_balance =
    match Map.find_opt (Tezos.sender, unfrozen_token_id) store.ledger with
      None -> (failwith("FA2_INSUFFICIENT_BALANCE") : ledger_value)
    | Some v -> v
    in
  if propose_params.frozen_token > current_balance
    then failwith("PROPOSAL_INSUFFICIENT_BALANCE")
    else ()

[@inline]
let check_proposal_limit_reached (config, propose_params, store : config * propose_params * storage): unit =
  if config.max_proposals <= List.length store.proposal_key_list_sort_by_date
    then failwith("MAX_PROPOSALS_REACHED")
    else ()

[@inline]
let freeze (tokens, addr, store : nat * address * storage): storage =
  let store = debit_from (tokens, addr, unfrozen_token_id, store) in
  let store = credit_to (tokens, addr, frozen_token_id, store) in
  store

[@inline]
let unfreeze (tokens, addr, store : nat * address * storage): storage =
  let store = debit_from (tokens, addr, frozen_token_id, store) in
  let store = credit_to (tokens, addr, unfrozen_token_id, store) in
  store

let add_proposal (propose_params, store : propose_params * storage): storage =
  let checked = ensure_proposal_is_unique (propose_params, store) in
  let timestamp = Tezos.now in
  let proposal : proposal =
    { upvotes = 0n
    ; downvotes = 0n
    ; start_date = timestamp
    ; metadata = propose_params.proposal_metadata
    ; proposer = Tezos.sender
    ; proposer_frozen_token = propose_params.frozen_token
    ; voters = ([] : (address * nat) list)
    } in
  let proposal_key = to_proposal_key(propose_params, Tezos.sender) in
  { store with
    proposals =
      Map.add proposal_key proposal store.proposals
  ; proposal_key_list_sort_by_date =
      Set.add (timestamp, proposal_key) store.proposal_key_list_sort_by_date
  }

let propose (config, param, store : config * propose_params * storage): return =
  let checked = check_is_proposal_valid (config, param, store) in
  let checked = check_proposal_limit_reached (config, param, store) in
  let checked = check_proposer_unfrozen_token (param, store) in

  let store = freeze (param.frozen_token, Tezos.sender, store) in
  let store = add_proposal (param, store) in
  ( ([] : operation list)
  , store
  )

// -----------------------------------------------------------------
// Vote
// -----------------------------------------------------------------

[@inline]
let check_voter_unfrozen_token (vote_param, author, store : vote_param * address * storage): unit =
  let current_balance =
    match Map.find_opt (author, unfrozen_token_id) store.ledger with
      None -> (failwith("FA2_INSUFFICIENT_BALANCE") : nat)
    | Some value -> value
    in
  if vote_param.vote_amount > current_balance
    then failwith("VOTING_INSUFFICIENT_BALANCE")
    else ()

let submit_vote (vote_param, author, store : vote_param * address * storage): storage =
  let proposal_key = vote_param.proposal_key in
  let proposal =
    match Map.find_opt proposal_key store.proposals with
      None -> (failwith("PROPOSAL_NOT_EXIST") : proposal)
    | Some p -> p
    in

  let store = freeze (vote_param.vote_amount, author, store) in

  let proposal =
    if vote_param.vote_type
      then { proposal with upvotes = proposal.upvotes + vote_param.vote_amount }
      else { proposal with downvotes = proposal.downvotes + vote_param.vote_amount }
    in
  let proposal =
    { proposal with
      voters = (author, vote_param.vote_amount) :: proposal.voters
    } in

  { store with
    proposals = Map.add proposal_key proposal store.proposals
  }

[@inline]
let check_vote_limit_reached
    (config, proposal, vote_param : config * proposal * vote_param): unit =
  if config.max_votes < proposal.upvotes + proposal.downvotes + vote_param.vote_amount
    then failwith("MAX_VOTES_REACHED")
    else ()

let vote(votes, config, store : vote_param_permited list * config * storage): return =
  let store = ensure_not_migrated store in
  let accept_vote = fun (store, pp : storage * vote_param_permited) ->
    let (param, author, store) = verify_permit_protected_vote (pp, store) in
    let proposal = check_if_proposal_exist (param.proposal_key, store) in
    let checked = check_vote_limit_reached (config, proposal, param) in
    let checked = ensure_voting_period_is_not_over (proposal, store) in
    let checked = check_voter_unfrozen_token (param, author, store) in
    let store = submit_vote (param, author, store) in
    store
    in
  ( ([] : operation list)
  , List.fold accept_vote votes store
  )

// -----------------------------------------------------------------
// Admin entrypoints
// -----------------------------------------------------------------

// Update voting period of all ongoing and new proposals.
[@inline]
let set_voting_period(new_period, config, store : voting_period * config * storage): return =
  let store = ensure_not_migrated store in
  let store = authorize_admin store in
  let checked =
    if   config.max_voting_period < new_period
      || config.min_voting_period > new_period
      then failwith("OUT_OF_BOUND_VOTING_PERIOD")
      else ()
    in
  let store = { store with voting_period = new_period } in
  (([] : operation list), store)

// Update quroum_threshold. The new quorum_threshold affects
// all ongoing and new proposals.
[@inline]
let set_quorum_threshold(new_threshold, config, store : quorum_threshold * config * storage): return =
  let store = ensure_not_migrated store in
  let store = authorize_admin store in
  let checked =
    if   config.max_quorum_threshold < new_threshold
      || config.min_quorum_threshold > new_threshold
      then failwith("OUT_OF_BOUND_QUORUM_THRESHOLD")
      else ()
    in
  let store = { store with quorum_threshold = new_threshold } in
  (([] : operation list), store)

// Used in "flash". See the Haskell version for explanation.
[@inline]
let check_balance_less_then_frozen_value
    (unfreeze_value, addr, proposal, proposal_key, store
      : nat * address * proposal * proposal_key * storage): nat =
  let actual_frozen_value =
    match Map.find_opt (addr, frozen_token_id) store.ledger with
      None -> (failwith("PROPOSER_NOT_EXIST_IN_LEDGER") : nat)
    | Some value -> value
    in
  if unfreeze_value > actual_frozen_value
    then actual_frozen_value
    else unfreeze_value

[@inline]
let burn_frozen_token (tokens, addr, store : nat * address * storage): storage =
  debit_from(tokens, addr, frozen_token_id, store)

// Burn the "slash_amount" calculated by "config.rejected_proposal_return_value".
// See the Haskell version for details.
[@inline]
let burn_slash_amount (slash_amount, frozen_tokens, addr, store : nat * nat * address * storage): storage =
  let to_burn =
    match Michelson.is_nat(frozen_tokens - slash_amount) with
      Some value_ -> slash_amount
    | None -> frozen_tokens
    in
  burn_frozen_token (to_burn, addr, store)

let unfreeze_proposer_token
  (config, is_accepted, proposal, proposal_key, store :
    config * bool * proposal * proposal_key * storage): storage =
  let (tokens, store) =
    if is_accepted
    then (proposal.proposer_frozen_token, store)
    else
      let slash_amount = config.rejected_proposal_return_value (proposal, store) in
      let frozen_tokens = proposal.proposer_frozen_token in
      let store = burn_slash_amount(slash_amount, frozen_tokens, proposal.proposer, store) in
      let tokens =
            match Michelson.is_nat(frozen_tokens - slash_amount) with
              Some value -> value
            | None -> 0n
            in
      (tokens, store)
    in
  let tokens = check_balance_less_then_frozen_value
              (tokens, proposal.proposer, proposal, proposal_key, store) in
  let store = unfreeze(tokens, proposal.proposer, store) in
  store

[@inline]
let unfreeze_voter_token
    (proposal, proposal_key, store : proposal * proposal_key * storage): storage =
  let do_unfreeze = fun (store, (addr, tokens) : storage * (address * nat)) ->
    unfreeze(tokens, addr, store)
    in
  List.fold do_unfreeze proposal.voters store

[@inline]
let is_voting_period_over (proposal, store : proposal * storage): bool =
  Tezos.now >= proposal.start_date + int(store.voting_period)

[@inline]
let is_counter_met (counter : counter): (bool * counter) =
  if counter.current >= counter.total
    then (true, counter)
    else (false, { counter with current = counter.current + 1n })

[@inline]
let do_total_vote_meet_quorum_threshold (proposal, store : proposal * storage): bool =
  store.quorum_threshold <= proposal.upvotes + proposal.downvotes

// Delete a proposal from 'sProposalKeyListSortByDate'
[@inline]
let delete_proposal
    (start_date, proposal_key, store : timestamp * proposal_key * storage): storage =
  { store with proposal_key_list_sort_by_date =
    Set.remove (start_date, proposal_key) store.proposal_key_list_sort_by_date
  }

[@inline]
let handle_proposal_is_over
    (config, start_date, proposal_key, store, ops, counter
      : config * timestamp * proposal_key * storage * operation list * counter
    )
    : (operation list * storage * counter) =
  let proposal = check_if_proposal_exist (proposal_key, store) in
  if is_voting_period_over (proposal, store)
  then
    let (finished, counter) = is_counter_met counter in
    if not finished
    then
      if do_total_vote_meet_quorum_threshold(proposal, store)
      then
        if proposal.upvotes > proposal.downvotes
        then
          let store = unfreeze_proposer_token (config, true, proposal, proposal_key, store) in
          let store = unfreeze_voter_token (proposal, proposal_key, store) in
          let (new_ops, store) = config.decision_lambda (proposal, store) in
          let cons = fun (l, e : operation list * operation) -> e :: l in
          let ops = List.fold cons ops new_ops in
          let store = delete_proposal (start_date, proposal_key, store) in
          (ops, store, counter)
        else
          let store = unfreeze_proposer_token (config, false, proposal, proposal_key, store) in
          let store = unfreeze_voter_token (proposal, proposal_key, store) in
          let store = delete_proposal (start_date, proposal_key, store) in
          (ops, store, counter)
      else
        let store = unfreeze_proposer_token (config, false, proposal, proposal_key, store) in
        let store = unfreeze_voter_token (proposal, proposal_key, store) in
        let store = delete_proposal (start_date, proposal_key, store) in
        (ops, store, counter)
    else (ops, store, counter)
  else (ops, store, counter)

// Flush all proposals that passed their voting period.
let flush(n, config, store : nat * config * storage): return =
  let store = ensure_not_migrated store in
  let store =
    if n = 0n
      then (failwith("BAD_ENTRYPOINT_PARAMETER") : storage)
      else store
    in

  let counter : counter =
    { current = 0n
    ; total = n
    } in
  let flush_one
      (acc, e: (operation list * storage * counter) * (timestamp * proposal_key)) =
        let (ops, store, counter) = acc in
        let (start_date, proposal_key) = e in
        handle_proposal_is_over (config, start_date, proposal_key, store, ops, counter)
      in
  let (ops, store, counter) =
    Set.fold flush_one store.proposal_key_list_sort_by_date (([] : operation list), store, counter) in
  (ops, store)

// Removes an accepted and finished proposal by key.
let drop_proposal (proposal_key, config, store : proposal_key * config * storage): return =
  let store = ensure_not_migrated store in
  let store = authorize_admin store in

  let proposal = check_if_proposal_exist (proposal_key, store) in
  if is_voting_period_over(proposal, store)
  then
    if do_total_vote_meet_quorum_threshold(proposal, store)
    then
      if proposal.upvotes > proposal.downvotes
      then
        let store = unfreeze_proposer_token (config, true, proposal, proposal_key, store) in
        let store = unfreeze_voter_token (proposal, proposal_key, store) in
        let store = delete_proposal (proposal.start_date, proposal_key, store) in
        (([] : operation list), store)
      else (failwith("FAIL_DROP_PROPOSAL_NOT_ACCEPTED") : return)
    else (failwith("FAIL_DROP_PROPOSAL_NOT_ACCEPTED") : return)
  else (failwith("FAIL_DROP_PROPOSAL_NOT_OVER") : return)

