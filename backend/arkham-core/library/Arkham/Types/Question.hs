module Arkham.Types.Question where

import Arkham.Prelude

import Arkham.Types.Card.Id
import Arkham.Types.Id
import {-# SOURCE #-} Arkham.Types.Message
import Arkham.Types.Target

data Question
    = ChooseOne [Message]
    | ChooseN Int [Message]
    | ChooseSome [Message]
    | ChooseUpToN Int [Message]
    | ChooseOneAtATime [Message]
    | -- | Choosing payment amounts
      -- The core idea is that costs get broken up into unitary costs and we
      -- let the players decide how many times an individual player will pay
      -- the cost. The @Maybe Int@ is used to designate whether or not there
      -- is a target value. The tuple of ints are the min and max bound for
      -- the specific investigator
      ChoosePaymentAmounts Text (Maybe Int) [(InvestigatorId, (Int, Int), Message)]
    | ChooseAmounts Text Int [(Text, (Int, Int))] Target
    | ChooseDynamicCardAmounts InvestigatorId CardId (Int, Int) Bool [Message] -- (Int, Int) is (min, max)
    | ChooseUpgradeDeck
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ChoosePlayerChoice = SetLeadInvestigator | SetTurnPlayer
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
