module Arkham.Investigator.Cards.MarieLambeau (
  marieLambeau,
  MarieLambeau (..),
)
where

import Arkham.Prelude

import Arkham.Action.Additional
import Arkham.Asset.Types (Field (..))
import Arkham.Event.Types (Field (..))
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Token

newtype MarieLambeau = MarieLambeau InvestigatorAttrs
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marieLambeau :: InvestigatorCard MarieLambeau
marieLambeau =
  investigator
    MarieLambeau
    Cards.marieLambeau
    Stats
      { health = 6
      , sanity = 8
      , willpower = 4
      , intellect = 4
      , combat = 1
      , agility = 3
      }

instance HasAbilities MarieLambeau where
  getAbilities (MarieLambeau _) = []

-- TODO: Allow for playing of Spell Cards
instance HasModifiersFor MarieLambeau where
  getModifiersFor target (MarieLambeau a) | isTarget a target = do
    hasDoom <- (> 0) <$> getDoomAmongstControlledCards (toId a)
    pure $
      toModifiers a [GiveAdditionalAction (TraitRestrictedAdditionalAction Spell NoRestriction) | hasDoom]
  getModifiersFor _ _ = pure []

getDoomAmongstControlledCards :: (HasGame m) => InvestigatorId -> m Int
getDoomAmongstControlledCards iid =
  getSum . fold
    <$> sequence
      [ selectAgg Sum AssetDoom (assetControlledBy iid)
      , selectAgg Sum EventDoom (eventControlledBy iid)
      , selectAgg Sum InvestigatorDoom (InvestigatorWithId iid)
      ]

instance HasChaosTokenValue MarieLambeau where
  getChaosTokenValue iid ElderSign (MarieLambeau attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage MarieLambeau where
  runMessage msg i@(MarieLambeau attrs) = case msg of
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      controlledAssets <- selectList (assetControlledBy iid)
      controlledEvents <- selectList (eventControlledBy iid)
      assetsWithDoom <- filterM (<=~> AssetWithAnyDoom) controlledAssets
      eventsWithDoom <- filterM (<=~> EventWithAnyDoom) controlledEvents

      let
        removeDoomTargets =
          map AssetTarget assetsWithDoom
            <> map EventTarget eventsWithDoom
            <> [InvestigatorTarget iid | investigatorDoom attrs > 0]

        addDoomTargets =
          map AssetTarget controlledAssets
            <> map EventTarget controlledEvents
            <> [InvestigatorTarget iid]

      push $
        chooseOne iid $
          Label
            "Add 1 doom"
            [ chooseOrRunOne
                iid
                [TargetLabel target [PlaceTokens (toSource attrs) target Doom 1] | target <- addDoomTargets]
            ]
            : [ Label
                "Remove 1 doom"
                [ chooseOrRunOne
                    iid
                    [TargetLabel target [RemoveTokens (toSource attrs) target Doom 1] | target <- removeDoomTargets]
                ]
              | notNull removeDoomTargets
              ]
              <> [Label "Do Nothing" []]
      pure i
    _ -> MarieLambeau <$> runMessage msg attrs
