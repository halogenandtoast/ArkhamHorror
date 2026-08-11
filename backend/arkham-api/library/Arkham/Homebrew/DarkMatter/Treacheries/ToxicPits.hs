module Arkham.Homebrew.DarkMatter.Treacheries.ToxicPits (toxicPits) where

import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message (pattern BeginSkillTest)
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type
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
      let skills = [#agility, #agility]
      push
        $ BeginSkillTest
        $ buildSkillTest
          sid
          iid
          attrs
          iid
          (AndSkillTest skills)
          (AndSkillBaseValue skills)
          (SkillTestDifficulty $ Fixed 3)
      skillTestModifier sid attrs sid RevealAnotherChaosToken
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      here <- select $ locationWithInvestigator iid
      for_ here \lid -> do
        investigators <- select $ investigatorAt lid
        for_ investigators \i -> assignDamage i attrs 1
        assets <- select $ AssetAtLocation lid
        for_ assets \a -> dealAssetDamage a attrs 1
        enemies <- select $ enemyAt lid
        for_ enemies \e -> nonAttackEnemyDamage Nothing attrs 1 e
      pure t
    _ -> ToxicPits <$> liftRunMessage msg attrs
