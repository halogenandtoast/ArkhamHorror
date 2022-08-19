{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
module Arkham.Question where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card.CardCode
import Arkham.ChaosBagStepState
import Arkham.Id
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Text
import Arkham.Window

data Component
  = InvestigatorComponent { investigatorId :: InvestigatorId , tokenType :: GameTokenType }
  | InvestigatorDeckComponent { investigatorId :: InvestigatorId }
  | AssetComponent { assetId :: AssetId, tokenType :: GameTokenType }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

data GameTokenType = ResourceToken | ClueToken | DamageToken | HorrorToken | DoomToken
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

data UI msg
  = Label { label :: Text, messages :: [msg] }
  | TooltipLabel { label :: Text, tooltip :: Tooltip, messages :: [msg] }
  | LabelGroup { label :: Text, messages :: [msg] }
  | CardLabel { cardCode :: CardCode, messages :: [msg] }
  | TargetLabel { target :: Target, messages :: [msg] }
  | SkillLabel { skillType :: SkillType, messages :: [msg] }
  | EvadeLabel { enemyId :: EnemyId, messages :: [msg] }
  | FightLabel { enemyId :: EnemyId, messages :: [msg] }
  | AbilityLabel { investigatorId :: InvestigatorId, ability :: Ability, windows :: [Window], messages :: [msg] }
  | ComponentLabel { component :: Component, messages :: [msg] }
  | EndTurnButton { investigatorId :: InvestigatorId, messages :: [msg] }
  | StartSkillTestButton { investigatorId :: InvestigatorId }
  | SkillTestApplyResultsButton
  | TokenGroupChoice { source :: Source, investigatorId ::  InvestigatorId, step :: ChaosBagStep }
  | Done { label :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PaymentAmountChoice msg = PaymentAmountChoice
  { investigatorId :: InvestigatorId
  , minBound :: Int
  , maxBound :: Int
  , message :: msg
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AmountChoice = AmountChoice
  { label :: Text
  , minBound :: Int
  , maxBound :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Question msg
  = ChooseOne { choices :: [UI msg] }
  | ChooseN { amount :: Int, choices :: [UI msg] }
  | ChooseSome { choices :: [UI msg] }
  | ChooseUpToN { amount :: Int, choices :: [UI msg] }
  | ChooseOneAtATime { choices :: [UI msg] }
  | -- | Choosing payment amounts
    -- The core idea is that costs get broken up into unitary costs and we
    -- let the players decide how many times an individual player will pay
    -- the cost. The @Maybe Int@ is used to designate whether or not there
    -- is a target value. The tuple of ints are the min and max bound for
    -- the specific investigator
    ChoosePaymentAmounts { label :: Text, paymentAmountTargetValue :: (Maybe Int), paymentAmountChoices :: [PaymentAmountChoice msg] }
  | ChooseAmounts { label :: Text, amountTargetValue ::  Int, amountChoices :: [AmountChoice], target :: Target }
  | ChooseUpgradeDeck
  | QuestionLabel { label :: Text, question :: (Question msg) }
  | Read { flavorText :: FlavorText, readChoices :: [UI msg] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChoosePlayerChoice = SetLeadInvestigator | SetTurnPlayer
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
