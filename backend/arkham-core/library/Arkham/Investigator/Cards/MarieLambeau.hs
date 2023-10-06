module Arkham.Investigator.Cards.MarieLambeau (
  marieLambeau,
  MarieLambeau (..),
)
where

import Arkham.Prelude

import Arkham.Action.Additional
import Arkham.Action.Additional qualified as Additional
import Arkham.Asset.Types (Field (..))
import Arkham.Classes.HasGame
import Arkham.Event.Types (Field (..))
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Token

newtype MarieLambeau = MarieLambeau InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marieAction :: AdditionalActionType
marieAction = TraitRestrictedAdditionalAction Spell Additional.NoRestriction

marieLambeau :: InvestigatorCard MarieLambeau
marieLambeau =
  investigator MarieLambeau Cards.marieLambeau
    $ Stats {health = 6, sanity = 8, willpower = 4, intellect = 4, combat = 1, agility = 3}

instance HasModifiersFor MarieLambeau where
  getModifiersFor target (MarieLambeau a) | a `is` target = do
    hasDoom <- (> 0) <$> getDoomAmongstControlledCards (toId a)
    pure
      $ toModifiers a
      $ [GiveAdditionalAction (AdditionalAction "Marie Lambeau" (toSource a) marieAction) | hasDoom]
  getModifiersFor _ _ = pure []

getDoomAmongstControlledCards :: HasGame m => InvestigatorId -> m Int
getDoomAmongstControlledCards iid =
  sumAllM
    [ selectAgg Sum AssetDoom (assetControlledBy iid)
    , selectAgg Sum EventDoom (eventControlledBy iid)
    , selectAgg Sum InvestigatorDoom (InvestigatorWithId iid)
    ]

instance HasChaosTokenValue MarieLambeau where
  getChaosTokenValue iid ElderSign (MarieLambeau attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage MarieLambeau where
  runMessage msg i@(MarieLambeau attrs) = case msg of
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      controlledAssets <- selectList $ assetControlledBy iid
      controlledEvents <- selectList $ eventControlledBy iid
      assetsWithDoom <- toTarget <$$> filterM (<=~> AssetWithAnyDoom) controlledAssets
      eventsWithDoom <- toTarget <$$> filterM (<=~> EventWithAnyDoom) controlledEvents

      let removes = assetsWithDoom <> eventsWithDoom <> [toTarget iid | investigatorDoom attrs > 0]
      let adds = map toTarget controlledAssets <> map toTarget controlledEvents <> [toTarget iid]
      let place target = PlaceTokens (toSource attrs) target Doom 1
      let remove target = RemoveTokens (toSource attrs) target Doom 1

      push
        $ chooseOne iid
        $ Label "Add 1 doom" [chooseOrRunOne iid $ targetLabels adds (only . place)]
        : ( mwhen (notNull removes)
              $ [Label "Remove 1 doom" [chooseOrRunOne iid $ targetLabels removes (only . remove)]]
          )
          <> [Label "Do Nothing" []]
      pure i
    _ -> MarieLambeau <$> runMessage msg attrs
