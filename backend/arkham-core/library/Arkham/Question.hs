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
import Arkham.Tarot
import Arkham.Text
import Arkham.Window
import Data.Aeson.TH

data Component
  = InvestigatorComponent {investigatorId :: InvestigatorId, tokenType :: GameTokenType}
  | InvestigatorDeckComponent {investigatorId :: InvestigatorId}
  | AssetComponent {assetId :: AssetId, tokenType :: GameTokenType}
  deriving stock (Show, Eq, Ord, Data)

data GameTokenType = ResourceToken | ClueToken | DamageToken | HorrorToken | DoomToken
  deriving stock (Show, Eq, Ord, Data)

pattern DamageLabel :: InvestigatorId -> [msg] -> UI msg
pattern DamageLabel iid msgs <- ComponentLabel (InvestigatorComponent iid DamageToken) msgs
  where
    DamageLabel iid msgs = ComponentLabel (InvestigatorComponent iid DamageToken) msgs

pattern AssetDamageLabel :: AssetId -> [msg] -> UI msg
pattern AssetDamageLabel aid msgs <- ComponentLabel (AssetComponent aid DamageToken) msgs
  where
    AssetDamageLabel aid msgs = ComponentLabel (AssetComponent aid DamageToken) msgs

pattern HorrorLabel :: InvestigatorId -> [msg] -> UI msg
pattern HorrorLabel iid msgs <- ComponentLabel (InvestigatorComponent iid HorrorToken) msgs
  where
    HorrorLabel iid msgs = ComponentLabel (InvestigatorComponent iid HorrorToken) msgs

pattern AssetHorrorLabel :: AssetId -> [msg] -> UI msg
pattern AssetHorrorLabel aid msgs <- ComponentLabel (AssetComponent aid HorrorToken) msgs
  where
    AssetHorrorLabel aid msgs = ComponentLabel (AssetComponent aid HorrorToken) msgs

pattern ResourceLabel :: InvestigatorId -> [msg] -> UI msg
pattern ResourceLabel iid msgs <- ComponentLabel (InvestigatorComponent iid ResourceToken) msgs
  where
    ResourceLabel iid msgs = ComponentLabel (InvestigatorComponent iid ResourceToken) msgs

data UI msg
  = Label {label :: Text, messages :: [msg]}
  | TooltipLabel {label :: Text, tooltip :: Tooltip, messages :: [msg]}
  | CardLabel {cardCode :: CardCode, messages :: [msg]}
  | PortraitLabel {investigatorId :: InvestigatorId, messages :: [msg]}
  | TargetLabel {target :: Target, messages :: [msg]}
  | SkillLabel {skillType :: SkillType, messages :: [msg]}
  | SkillLabelWithLabel {label :: Text, skillType :: SkillType, messages :: [msg]}
  | EvadeLabel {enemyId :: EnemyId, messages :: [msg]}
  | FightLabel {enemyId :: EnemyId, messages :: [msg]}
  | EngageLabel {enemyId :: EnemyId, messages :: [msg]}
  | GridLabel {gridLabel :: Text, messages :: [msg]}
  | TarotLabel {tarotCard :: TarotCard, messages :: [msg]}
  | AbilityLabel
      {investigatorId :: InvestigatorId, ability :: Ability, windows :: [Window], messages :: [msg]}
  | ComponentLabel {component :: Component, messages :: [msg]}
  | EndTurnButton {investigatorId :: InvestigatorId, messages :: [msg]}
  | StartSkillTestButton {investigatorId :: InvestigatorId}
  | SkillTestApplyResultsButton
  | ChaosTokenGroupChoice {source :: Source, investigatorId :: InvestigatorId, step :: ChaosBagStep}
  | EffectActionButton {tooltip :: Tooltip, effectId :: EffectId, messages :: [msg]}
  | Done {label :: Text}
  | SkipTriggersButton {investigatorId :: InvestigatorId}
  deriving stock (Show, Eq, Data)

data PaymentAmountChoice msg = PaymentAmountChoice
  { investigatorId :: InvestigatorId
  , minBound :: Int
  , maxBound :: Int
  , title :: Text
  , message :: msg
  }
  deriving stock (Show, Eq, Data)

data AmountChoice = AmountChoice
  { label :: Text
  , minBound :: Int
  , maxBound :: Int
  }
  deriving stock (Show, Eq, Data)

data AmountTarget = MinAmountTarget Int | MaxAmountTarget Int | TotalAmountTarget Int | AmountOneOf [Int]
  deriving stock (Show, Eq, Data)

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
      , paymentAmountTargetValue :: Maybe AmountTarget
      , paymentAmountChoices :: [PaymentAmountChoice msg]
      }
  | ChooseAmounts
      { label :: Text
      , amountTargetValue :: AmountTarget
      , amountChoices :: [AmountChoice]
      , target :: Target
      }
  | ChooseUpgradeDeck
  | ChooseDeck
  | QuestionLabel {label :: Text, card :: Maybe CardCode, question :: Question msg}
  | Read {flavorText :: FlavorText, readChoices :: [UI msg]}
  | PickSupplies {pointsRemaining :: Int, chosenSupplies :: [Supply], choices :: [UI msg]}
  | DropDown {options :: [(Text, msg)]}
  | PickScenarioSettings
  | PickCampaignSettings
  deriving stock (Show, Eq, Data)

data ChoosePlayerChoice = SetLeadInvestigator | SetTurnPlayer
  deriving stock (Show, Eq, Data)

targetLabel
  :: (Targetable target, msg ~ Element (t msg), MonoFoldable (t msg))
  => target
  -> t msg
  -> UI msg
targetLabel (toTarget -> target) (toList -> msgs) = TargetLabel target msgs

targetLabels
  :: (Targetable target, msg ~ Element (t msg), MonoFoldable (t msg))
  => [target]
  -> (target -> t msg)
  -> [UI msg]
targetLabels = flip mapTargetLabel

mapTargetLabel
  :: (Targetable target, msg ~ Element (t msg), MonoFoldable (t msg))
  => (target -> t msg)
  -> [target]
  -> [UI msg]
mapTargetLabel f = map (\c -> targetLabel c (f c))

mapTargetLabelWith :: Targetable target => (c -> target) -> (c -> [msg]) -> [c] -> [UI msg]
mapTargetLabelWith g f = map (uncurry targetLabel . (g &&& f))

$(deriveJSON defaultOptions ''GameTokenType)
$(deriveJSON defaultOptions ''Component)
$(deriveJSON defaultOptions ''PaymentAmountChoice)
$(deriveJSON defaultOptions ''ChoosePlayerChoice)
$(deriveJSON defaultOptions ''AmountChoice)
$(deriveJSON defaultOptions ''AmountTarget)
$(deriveJSON defaultOptions ''UI)
$(deriveJSON defaultOptions ''Question)
