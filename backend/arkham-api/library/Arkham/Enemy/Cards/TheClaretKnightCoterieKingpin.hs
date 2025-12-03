module Arkham.Enemy.Cards.TheClaretKnightCoterieKingpin (theClaretKnightCoterieKingpin) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype TheClaretKnightCoterieKingpin = TheClaretKnightCoterieKingpin EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theClaretKnightCoterieKingpin :: EnemyCard TheClaretKnightCoterieKingpin
theClaretKnightCoterieKingpin =
  enemy TheClaretKnightCoterieKingpin Cards.theClaretKnightCoterieKingpin (4, PerPlayer 4, 3) (1, 2)

instance HasAbilities TheClaretKnightCoterieKingpin where
  getAbilities (TheClaretKnightCoterieKingpin a) =
    extend1 a
      $ restricted a 1 (thisExists a $ EnemyWithScarletKey ScarletKeyAny)
      $ forced
      $ SkillTestResult #after You (SkillTestAt $ locationWithEnemy a) #failure

instance RunMessage TheClaretKnightCoterieKingpin where
  runMessage msg e@(TheClaretKnightCoterieKingpin attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      skeys <- select $ ScarletKeyWithPlacement (AttachedToEnemy attrs.id)
      lead <- getLead
      chooseOneAtATimeM lead $ targets skeys shift
      pure e
    _ -> TheClaretKnightCoterieKingpin <$> liftRunMessage msg attrs
