module Arkham.Homebrew.TheMasqueOfTheRedDeath.Treacheries.Debauchery (debauchery) where

import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Import.Lifted

newtype Debauchery = Debauchery TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

debauchery :: TreacheryCard Debauchery
debauchery = treachery Debauchery Cards.debauchery

cultistEnemy :: EnemyMatcher
cultistEnemy = EnemyWithTrait Cultist

cultistAsset :: AssetMatcher
cultistAsset = AssetWithTrait Cultist <> StoryAsset

instance RunMessage Debauchery where
  runMessage msg t@(Debauchery attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      furthest <-
        select
          $ cultistEnemy
          <> EnemyAt (FarthestLocationFromInvestigator (be iid) (LocationWithEnemy cultistEnemy))
      nearest <- select $ cultistAsset <> AssetAt (NearestLocationTo iid (LocationWithAsset cultistAsset))
      chooseOneM iid $ campaignI18n $ scope "debauchery" do
        -- A targeting effect with no legal target cannot be chosen (Grimoire,
        -- glossary/effects), so the enemy half drops when no Cultist enemy is in
        -- play. The other half always resolves at least its damage or horror.
        when (notNull furthest)
          $ labeled "doomOnFurthestCultistEnemy"
          $ chooseTargetM iid furthest \enemy -> placeDoom attrs enemy 1
        labeled "damageOrHorrorAndDoomOnNearestCultistAsset" do
          assignDamageOrHorror iid attrs 1 1
          chooseTargetM iid nearest \asset -> placeDoom attrs asset 1
      pure t
    _ -> Debauchery <$> liftRunMessage msg attrs
