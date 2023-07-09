{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Question where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card.CardCode
import Arkham.ChaosBagStepState
import Arkham.Id
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Text
import Arkham.Window
import Data.Aeson.TH

data Component
  = InvestigatorComponent {investigatorId :: InvestigatorId, tokenType :: GameTokenType}
  | InvestigatorDeckComponent {investigatorId :: InvestigatorId}
  | AssetComponent {assetId :: AssetId, tokenType :: GameTokenType}
  deriving stock (Show, Eq, Ord)

data GameTokenType = ResourceToken | ClueToken | DamageToken | HorrorToken | DoomToken
  deriving stock (Show, Eq, Ord)

data UI msg
  = Label {label :: Text, messages :: [msg]}
  | TooltipLabel {label :: Text, tooltip :: Tooltip, messages :: [msg]}
  | CardLabel {cardCode :: CardCode, messages :: [msg]}
  | PortraitLabel {investigatorId :: InvestigatorId, messages :: [msg]}
  | TargetLabel {target :: Target, messages :: [msg]}
  | SkillLabel {skillType :: SkillType, messages :: [msg]}
  | EvadeLabel {enemyId :: EnemyId, messages :: [msg]}
  | FightLabel {enemyId :: EnemyId, messages :: [msg]}
  | AbilityLabel
      {investigatorId :: InvestigatorId, ability :: Ability, windows :: [Window], messages :: [msg]}
  | ComponentLabel {component :: Component, messages :: [msg]}
  | EndTurnButton {investigatorId :: InvestigatorId, messages :: [msg]}
  | StartSkillTestButton {investigatorId :: InvestigatorId}
  | SkillTestApplyResultsButton
  | TokenGroupChoice {source :: Source, investigatorId :: InvestigatorId, step :: ChaosBagStep}
  | EffectActionButton {tooltip :: Tooltip, effectId :: EffectId, messages :: [msg]}
  | Done {label :: Text}
  deriving stock (Show, Eq, Ord)

data PaymentAmountChoice msg = PaymentAmountChoice
  { investigatorId :: InvestigatorId
  , minBound :: Int
  , maxBound :: Int
  , message :: msg
  }
  deriving stock (Show, Eq)

data AmountChoice = AmountChoice
  { label :: Text
  , minBound :: Int
  , maxBound :: Int
  }
  deriving stock (Show, Eq)

data AmountTarget = MaxAmountTarget Int | TotalAmountTarget Int
  deriving stock (Show, Eq)

data Question msg
  = ChooseOne {choices :: [UI msg]}
  | ChooseN {amount :: Int, choices :: [UI msg]}
  | ChooseSome {choices :: [UI msg]}
  | ChooseSome1 {label :: Text, choices :: [UI msg]}
  | ChooseUpToN {amount :: Int, choices :: [UI msg]}
  | ChooseOneAtATime {choices :: [UI msg]}
  | -- | Choosing payment amounts
    -- The core idea is that costs get broken up into unitary costs and we
    -- let the players decide how many times an individual player will pay
    -- the cost. The @Maybe Int@ is used to designate whether or not there
    -- is a target value. The tuple of ints are the min and max bound for
    -- the specific investigator
    ChoosePaymentAmounts
      { label :: Text
      , paymentAmountTargetValue :: (Maybe Int)
      , paymentAmountChoices :: [PaymentAmountChoice msg]
      }
  | ChooseAmounts
      { label :: Text
      , amountTargetValue :: AmountTarget
      , amountChoices :: [AmountChoice]
      , target :: Target
      }
  | ChooseUpgradeDeck
  | QuestionLabel {label :: Text, question :: (Question msg)}
  | Read {flavorText :: FlavorText, readChoices :: [UI msg]}
  | PickSupplies {pointsRemaining :: Int, chosenSupplies :: [Supply], choices :: [UI msg]}
  | DropDown {options :: [(Text, msg)]}
  deriving stock (Show, Eq)

data ChoosePlayerChoice = SetLeadInvestigator | SetTurnPlayer
  deriving stock (Show, Eq)

targetLabel :: (Targetable target) => target -> [msg] -> UI msg
targetLabel = TargetLabel . toTarget

mapTargetLabel :: (Targetable target) => (target -> [msg]) -> [target] -> [UI msg]
mapTargetLabel f = map (\c -> targetLabel c (f c))

mapTargetLabelWith :: (Targetable target) => (c -> target) -> (c -> [msg]) -> [c] -> [UI msg]
mapTargetLabelWith g f = map (uncurry targetLabel . (g &&& f))

$(deriveJSON defaultOptions ''GameTokenType)
$(deriveJSON defaultOptions ''Component)
$(deriveJSON defaultOptions ''PaymentAmountChoice)
$(deriveJSON defaultOptions ''ChoosePlayerChoice)
$(deriveJSON defaultOptions ''AmountChoice)
$(deriveJSON defaultOptions ''AmountTarget)
$(deriveJSON defaultOptions ''UI)
$(deriveJSON defaultOptions ''Question)
