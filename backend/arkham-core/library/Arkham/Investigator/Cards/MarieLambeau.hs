module Arkham.Investigator.Cards.MarieLambeau (
  marieLambeau,
  MarieLambeau (..),
)
where

import Arkham.Prelude

import Arkham.Action.Additional
import Arkham.Action.Additional qualified as Additional
import Arkham.Asset.Types (Field (..))
import Arkham.Event.Types (Field (..))
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Token

newtype Meta = Meta {additionalActionActive :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MarieLambeau = MarieLambeau (InvestigatorAttrs `With` Meta)
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pattern MarieAction :: AdditionalAction
pattern MarieAction <- TraitRestrictedAdditionalAction Spell Additional.NoRestriction
  where
    MarieAction = TraitRestrictedAdditionalAction Spell Additional.NoRestriction

marieLambeau :: InvestigatorCard MarieLambeau
marieLambeau =
  investigator (MarieLambeau . (`with` Meta False)) Cards.marieLambeau
    $ Stats {health = 6, sanity = 8, willpower = 4, intellect = 4, combat = 1, agility = 3}

-- TODO: This modifier won't change if doom is added during their turn
instance HasModifiersFor MarieLambeau where
  getModifiersFor target (MarieLambeau (a `With` _)) | a `is` target = do
    hasDoom <- (> 0) <$> getDoomAmongstControlledCards (toId a)
    pure $ toModifiers a $ [GiveAdditionalAction MarieAction | hasDoom]
  getModifiersFor _ _ = pure []

getDoomAmongstControlledCards :: HasGame m => InvestigatorId -> m Int
getDoomAmongstControlledCards iid =
  sumAllM
    [ selectAgg Sum AssetDoom (assetControlledBy iid)
    , selectAgg Sum EventDoom (eventControlledBy iid)
    , selectAgg Sum InvestigatorDoom (InvestigatorWithId iid)
    ]

instance HasChaosTokenValue MarieLambeau where
  getChaosTokenValue iid ElderSign (MarieLambeau (attrs `With` _)) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

checkForAction :: (HasGame m, HasQueue Message m) => InvestigatorAttrs -> Meta -> m ()
checkForAction attrs meta = do
  hasDoom <- (> 0) <$> getDoomAmongstControlledCards (toId attrs)

  pushWhen (hasDoom && not (additionalActionActive meta))
    $ GainAdditionalAction (toId attrs) (toSource attrs) MarieAction
  pushWhen (not hasDoom && additionalActionActive meta)
    $ LoseAdditionalAction (toId attrs) (toSource attrs) MarieAction

instance RunMessage MarieLambeau where
  runMessage msg i@(MarieLambeau (attrs `With` meta)) = case msg of
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
    CheckWindow iids _ | toId attrs `elem` iids -> do
      attrs' <- runMessage msg attrs
      checkForAction attrs' meta
      pure $ MarieLambeau $ attrs' `with` meta
    GainAdditionalAction iid (isSource attrs -> True) MarieAction | attrs `is` iid -> do
      attrs' <- runMessage msg attrs
      pure $ MarieLambeau $ attrs' `with` Meta True
    LoseAdditionalAction iid (isSource attrs -> True) MarieAction | attrs `is` iid -> do
      attrs' <- runMessage msg attrs
      pure $ MarieLambeau $ attrs' `with` Meta False
    PlayerWindow iid _ _ | attrs `is` iid -> do
      attrs' <- runMessage msg attrs
      checkForAction attrs' meta
      pure $ MarieLambeau $ attrs' `with` meta
    _ -> MarieLambeau . (`with` meta) <$> runMessage msg attrs
