{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Question where

import Arkham.Prelude hiding (maxBound, minBound)

import Arkham.Ability.Types
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.ChaosBagStepState
import Arkham.Id
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Tarot
import Arkham.Text
import Arkham.Window
import Control.Monad.Fail
import Data.Aeson.TH
import Data.UUID (nil)

data Component
  = InvestigatorComponent {investigatorId :: InvestigatorId, tokenType :: GameTokenType}
  | InvestigatorDeckComponent {investigatorId :: InvestigatorId}
  | AssetComponent {assetId :: AssetId, tokenType :: GameTokenType}
  deriving stock (Show, Eq, Ord, Data)

data GameTokenType = ResourceToken | ClueToken | DamageToken | HorrorToken | DoomToken
  deriving stock (Show, Eq, Ord, Data)

pattern ClueLabel :: InvestigatorId -> [msg] -> UI msg
pattern ClueLabel iid msgs <- ComponentLabel (InvestigatorComponent iid ClueToken) msgs
  where
    ClueLabel iid msgs = ComponentLabel (InvestigatorComponent iid ClueToken) msgs

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
  | InvalidLabel { label :: Text }
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
      { investigatorId :: InvestigatorId
      , ability :: Ability
      , windows :: [Window]
      , before :: [msg]
      , messages :: [msg]
      }
  | ComponentLabel {component :: Component, messages :: [msg]}
  | EndTurnButton {investigatorId :: InvestigatorId, messages :: [msg]}
  | StartSkillTestButton {investigatorId :: InvestigatorId}
  | SkillTestApplyResultsButton
  | ChaosTokenGroupChoice {source :: Source, investigatorId :: InvestigatorId, step :: ChaosBagStep}
  | EffectActionButton {tooltip :: Tooltip, effectId :: EffectId, messages :: [msg]}
  | Done {label :: Text}
  | SkipTriggersButton {investigatorId :: InvestigatorId}
  | CardPile {pile :: [PileCard], messages :: [msg]}
  deriving stock (Show, Eq, Data)

data PileCard = PileCard
  { cardId :: CardId
  , cardOwner :: Maybe InvestigatorId
  }
  deriving stock (Show, Eq, Data)

data PaymentAmountChoice msg = PaymentAmountChoice
  { choiceId :: UUID
  , investigatorId :: InvestigatorId
  , minBound :: Int
  , maxBound :: Int
  , title :: Text
  , message :: msg
  }
  deriving stock (Show, Eq, Data)

data AmountChoice = AmountChoice
  { choiceId :: UUID
  , label :: Text
  , minBound :: Int
  , maxBound :: Int
  }
  deriving stock (Show, Eq, Data)

data AmountTarget = MinAmountTarget Int | MaxAmountTarget Int | TotalAmountTarget Int | AmountOneOf [Int]
  deriving stock (Show, Eq, Data)

data Question msg
  = ChooseOne {choices :: [UI msg]}
  | PlayerWindowChooseOne {choices :: [UI msg]}
  | ChooseOneFromEach {groups :: [[UI msg]]}
  | ChooseN {amount :: Int, choices :: [UI msg]}
  | ChooseSome {choices :: [UI msg]}
  | ChooseSome1 {label :: Text, choices :: [UI msg]}
  | ChooseUpToN {amount :: Int, choices :: [UI msg]}
  | ChooseOneAtATime {choices :: [UI msg]}
  | ChooseOneAtATimeWithAuto {label :: Text, choices :: [UI msg]}
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
  | Read {flavorText :: FlavorText, readChoices :: ReadChoices msg, readCards :: Maybe [CardCode]}
  | PickSupplies {pointsRemaining :: Int, chosenSupplies :: [Supply], choices :: [UI msg]}
  | DropDown {options :: [(Text, msg)]}
  | PickScenarioSettings
  | PickCampaignSettings
  deriving stock (Show, Eq, Data)

data ReadChoices msg
  = BasicReadChoices [UI msg]
  | BasicReadChoicesN Int [UI msg]
  | BasicReadChoicesUpToN Int [UI msg]
  | LeadInvestigatorMustDecide [UI msg]
  deriving stock (Show, Eq, Data)

data ChoosePlayerChoice = SetLeadInvestigator | SetTurnPlayer
  deriving stock (Show, Eq, Data)

evadeLabel
  :: (AsId enemy, IdOf enemy ~ EnemyId,  msg ~ Element (t msg), MonoFoldable (t msg))
  => enemy
  -> t msg
  -> UI msg
evadeLabel (asId -> enemy) (toList -> msgs) = EvadeLabel enemy msgs

fightLabel
  :: (AsId enemy, IdOf enemy ~ EnemyId,  msg ~ Element (t msg), MonoFoldable (t msg))
  => enemy
  -> t msg
  -> UI msg
fightLabel (asId -> enemy) (toList -> msgs) = FightLabel enemy msgs

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

concat
  [ deriveJSON defaultOptions ''GameTokenType
  , deriveJSON defaultOptions ''Component
  , deriveToJSON defaultOptions ''PaymentAmountChoice
  , [d|
      instance FromJSON msg => FromJSON (PaymentAmountChoice msg) where
        parseJSON = withObject "PaymentAmountChoice" $ \o -> do
          choiceId <- o .:? "choiceId" .!= nil
          investigatorId <- o .: "investigatorId"
          minBound <- o .: "minBound"
          maxBound <- o .: "maxBound"
          title <- o .: "title"
          message <- o .: "message"
          pure PaymentAmountChoice {..}
      |]
  , deriveJSON defaultOptions ''ChoosePlayerChoice
  , deriveToJSON defaultOptions ''AmountChoice
  , [d|
      instance FromJSON AmountChoice where
        parseJSON = withObject "AmountChoice" $ \o -> do
          choiceId <- o .:? "choiceId" .!= nil
          label <- o .: "label"
          minBound <- o .: "minBound"
          maxBound <- o .: "maxBound"
          pure AmountChoice {..}
      |]
  , deriveJSON defaultOptions ''AmountTarget
  , deriveJSON defaultOptions ''PileCard
  , deriveToJSON defaultOptions ''ReadChoices
  , [d|
      instance FromJSON msg => FromJSON (ReadChoices msg) where
        parseJSON (Array xs) = BasicReadChoices <$> parseJSON (Array xs)
        parseJSON (Object o) = $(mkParseJSON defaultOptions ''ReadChoices) (Object o)
        parseJSON other = fail $ "Unexpected json type: " <> show other
      |]
  , deriveJSON defaultOptions ''UI
  , deriveJSON defaultOptions ''Question
  ]
