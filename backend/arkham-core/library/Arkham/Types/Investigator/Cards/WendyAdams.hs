module Arkham.Types.Investigator.Cards.WendyAdams
  ( WendyAdams(..)
  , wendyAdams
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.EntityInstance
import Arkham.Types.Game.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Message hiding (RevealToken)
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype WendyAdams = WendyAdams InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

wendyAdams :: WendyAdams
wendyAdams = WendyAdams $ baseAttrs
  "01005"
  "Wendy Adams"
  Survivor
  Stats
    { health = 7
    , sanity = 7
    , willpower = 4
    , intellect = 3
    , combat = 1
    , agility = 4
    }
  [Drifter]

instance HasTokenValue env WendyAdams where
  getTokenValue (WendyAdams attrs) iid ElderSign | iid == investigatorId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 0)
  getTokenValue (WendyAdams attrs) iid token = getTokenValue attrs iid token

ability :: InvestigatorAttrs -> Token -> Ability
ability attrs token = base
  { abilityLimit = PlayerLimit PerTestOrAbility 1
  , abilityMetadata = Just (TargetMetadata $ TokenTarget token)
  }
 where
  base = mkAbility
    (toSource attrs)
    1
    (LegacyReactionAbility $ HandDiscardCost 1 Nothing mempty mempty)

instance EntityInstanceRunner env => HasAbilities env WendyAdams where
  getAbilities iid (Window Timing.When (Window.RevealToken who token)) (WendyAdams attrs@InvestigatorAttrs {..})
    | iid == investigatorId && iid == who
    = pure [ability attrs token]
  getAbilities i window (WendyAdams attrs) = getAbilities i window attrs

instance (InvestigatorRunner env) => RunMessage env WendyAdams where
  runMessage msg i@(WendyAdams attrs@InvestigatorAttrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) [Window _ (Window.RevealToken _ token)] 1 _
      | iid == investigatorId
      -> do
        cancelToken token
        i <$ pushAll
          [ CancelNext RunWindowMessage
          , CancelNext DrawTokenMessage
          , CancelNext RevealTokenMessage
          , ReturnTokens [token]
          , UnfocusTokens
          , DrawAnotherToken iid
          ]
    When (DrawToken iid token) | iid == investigatorId -> i <$ pushAll
      [ FocusTokens [token]
      , CheckWindow
        investigatorId
        [Window Timing.When (Window.DrawToken investigatorId token)]
      , UnfocusTokens
      ]
    ResolveToken _drawnToken ElderSign iid | iid == investigatorId -> do
      maid <- getId @(Maybe AssetId) (CardCode "01014")
      i <$ when (isJust maid) (push PassSkillTest)
    _ -> WendyAdams <$> runMessage msg attrs
