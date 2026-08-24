module Arkham.Homebrew.DarkMatter.Treacheries.ToxicPits (toxicPits) where

import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest.Lifted (combinationSkillTestEdit)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.SkillTest.Base (setIsRevelation)
import Arkham.Treachery.Import.Lifted

newtype ToxicPits = ToxicPits TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toxicPits :: TreacheryCard ToxicPits
toxicPits = treachery ToxicPits Cards.toxicPits

{- | "Revelation - Test [agility] + [agility] (3). Reveal and resolve an
additional chaos token for this skill test. If you fail, deal 1 damage to each
investigator, asset, and enemy at your location."
-}
instance RunMessage ToxicPits where
  runMessage msg t@(ToxicPits attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      -- the reveal strategy is read when the test triggers, so arm it first
      skillTestModifier sid attrs sid RevealAnotherChaosToken
      combinationSkillTestEdit sid iid attrs iid [#agility, #agility] (Fixed 3) setIsRevelation
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      here <- select $ locationWithInvestigator iid
      for_ here \lid -> do
        investigators <- select $ investigatorAt lid
        for_ investigators \i -> assignDamage i attrs 1
        assets <- select $ at_ (LocationWithId lid) <> AssetWithHealth
        for_ assets \a -> dealAssetDamage a attrs 1
        enemies <- select $ enemyAt lid <> EnemyCanBeDamagedBySource (toSource attrs)
        for_ enemies $ nonAttackEnemyDamage Nothing attrs 1
      pure t
    _ -> ToxicPits <$> liftRunMessage msg attrs
