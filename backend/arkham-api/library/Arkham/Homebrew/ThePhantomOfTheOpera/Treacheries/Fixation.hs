module Arkham.Homebrew.ThePhantomOfTheOpera.Treacheries.Fixation (fixation) where

import Arkham.Ability
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype Fixation = Fixation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fixation :: TreacheryCard Fixation
fixation = treachery Fixation Cards.fixation

instance HasAbilities Fixation where
  getAbilities (Fixation a) = case a.placement of
    AttachedToAsset aid _ ->
      [mkAbility a 1 $ forced $ ActivateAbility #after You (AbilityOnAsset $ AssetWithId aid)]
    _ -> []

instance RunMessage Fixation where
  runMessage msg t@(Fixation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <-
        select
          $ AssetWithHighestPrintedCost (assetControlledBy iid <> AssetNonStory <> not_ PermanentAsset)
      if null assets
        then gainSurge attrs
        else chooseOrRunOneM iid $ targets assets $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      phantom <- selectOne $ mapOneOf enemyIs [Enemies.thePhantom, Enemies.thePhantomTragicFigure]
      scenarioI18n $ scope "fixation" $ chooseOneM iid do
        unscoped $ labeled "discardThatAsset" $ for_ attrs.attached $ toDiscardBy iid (attrs.ability 1)
        for_ phantom \eid ->
          labeled "thePhantomAttacks" $ initiateEnemyAttack eid (attrs.ability 1) iid
      pure t
    _ -> Fixation <$> liftRunMessage msg attrs
