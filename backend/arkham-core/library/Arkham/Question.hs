module Arkham.Question where

import Arkham.Prelude

import Arkham.Id
import Arkham.Target
import Arkham.Text

data Question msg
    = ChooseOne [msg]
    | ChooseN Int [msg]
    | ChooseSome [msg]
    | ChooseUpToN Int [msg]
    | ChooseOneAtATime [msg]
    | -- | Choosing payment amounts
      -- The core idea is that costs get broken up into unitary costs and we
      -- let the players decide how many times an individual player will pay
      -- the cost. The @Maybe Int@ is used to designate whether or not there
      -- is a target value. The tuple of ints are the min and max bound for
      -- the specific investigator
      ChoosePaymentAmounts Text (Maybe Int) [(InvestigatorId, (Int, Int), msg)]
    | ChooseAmounts Text Int [(Text, (Int, Int))] Target
    | ChooseUpgradeDeck
    | QuestionLabel Text (Question msg)
    | Read FlavorText [(Text, msg)]
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ChoosePlayerChoice = SetLeadInvestigator | SetTurnPlayer
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
