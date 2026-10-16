module Arkham.Homebrew.TheLegendOfSleepyHollow.Treacheries.AccursedHollow (accursedHollow) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Matcher
import Arkham.Trait (Trait (Geist))
import Arkham.Treachery.Import.Lifted

newtype AccursedHollow = AccursedHollow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

accursedHollow :: TreacheryCard AccursedHollow
accursedHollow = treachery AccursedHollow Cards.accursedHollow

{- | "The attached location gains \"Haunted - Place 1 doom on Accursed
Hollow.\"" Proxied onto the location so the failed-investigate resolution finds
it there.
-}
instance HasAbilities AccursedHollow where
  getAbilities (AccursedHollow a) = case a.attached.location of
    Nothing -> []
    Just lid ->
      [ scenarioI18n $ hauntedI "accursedHollow.haunted" (proxied (LocationSource lid) a) 1
      , scenarioI18n
          $ withI18nTooltip "accursedHollow.banish"
          $ mkAbility a 2
          $ freeReaction
          $ IfEnemyDefeated #after You ByAny (EnemyWithTrait Geist <> enemyWasAt lid)
      ]

instance RunMessage AccursedHollow where
  runMessage msg t@(AccursedHollow attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        alreadyThere <- selectAny $ treacheryAt lid <> treacheryIs Cards.accursedHollow
        if alreadyThere then gainSurge attrs else attachTreachery attrs lid
      pure t
    UseThisAbility _ (isProxySource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> AccursedHollow <$> liftRunMessage msg attrs
