module Arkham.Question where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card.CardCode
import Arkham.Id
import Arkham.SkillType
import Arkham.Target
import Arkham.Text
import Arkham.Window

data Component
  = InvestigatorComponent InvestigatorId GameTokenType
  | InvestigatorDeckComponent InvestigatorId
  | AssetComponent AssetId GameTokenType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

data GameTokenType = ResourceToken | ClueToken | DamageToken | HorrorToken | DoomToken
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

data UI msg
  = Label Text [msg]
  | TooltipLabel Text Tooltip [msg]
  | LabelGroup Text [msg]
  | CardLabel CardCode [msg]
  | TargetLabel Target [msg]
  | SkillLabel SkillType [msg]
  | EvadeLabel EnemyId [msg]
  | AbilityLabel InvestigatorId Ability [Window] [msg]
  | ComponentLabel Component [msg]
  | EndTurnButton InvestigatorId [msg]
  | Done Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Question msg
    = ChooseOne [UI msg]
    | ChooseN Int [UI msg]
    | ChooseSome [UI msg]
    | ChooseUpToN Int [UI msg]
    | ChooseOneAtATime [UI msg]
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
