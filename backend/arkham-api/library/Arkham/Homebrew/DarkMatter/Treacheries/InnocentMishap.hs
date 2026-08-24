module Arkham.Homebrew.DarkMatter.Treacheries.InnocentMishap (innocentMishap) where

import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n, nearestBrain)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype InnocentMishap = InnocentMishap TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innocentMishap :: TreacheryCard InnocentMishap
innocentMishap = treachery InnocentMishap Cards.innocentMishap

{- | "Peril. / Revelation - You must either (choose one):
- Deal 1 damage to the nearest [[Brain]] story asset.
- Deal 2 damage to Mi-Go Scientist.
- Take 2 direct damage."
-}
instance RunMessage InnocentMishap where
  runMessage msg t@(InnocentMishap attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      brains <- select $ nearestBrain iid
      scientist <- select $ enemyIs Enemies.miGoScientist
      chooseOneM iid $ campaignI18n do
        unless (null brains) $ labeled' "innocentMishap.damageNearestBrain" do
          chooseTargetM iid brains \brain -> dealAssetDamage brain attrs 1
        unless (null scientist) $ labeled' "innocentMishap.damageMiGoScientist" do
          for_ scientist $ nonAttackEnemyDamage Nothing attrs 2
        labeled' "innocentMishap.take2DirectDamage" $ directDamage iid attrs 2
      pure t
    _ -> InnocentMishap <$> liftRunMessage msg attrs
