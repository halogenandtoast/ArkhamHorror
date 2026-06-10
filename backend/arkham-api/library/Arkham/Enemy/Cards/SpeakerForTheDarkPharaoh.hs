module Arkham.Enemy.Cards.SpeakerForTheDarkPharaoh (speakerForTheDarkPharaoh) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype SpeakerForTheDarkPharaoh = SpeakerForTheDarkPharaoh EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

speakerForTheDarkPharaoh :: EnemyCard SpeakerForTheDarkPharaoh
speakerForTheDarkPharaoh = enemy SpeakerForTheDarkPharaoh Cards.speakerForTheDarkPharaoh (2, Static 2, 3) (0, 1)

instance HasModifiersFor SpeakerForTheDarkPharaoh where
  getModifiersFor (SpeakerForTheDarkPharaoh a) = do
    n <- getStrengthOfTheAbyss
    modifySelfWhen a (n <= 2) [AddKeyword Keyword.Hunter]

instance HasAbilities SpeakerForTheDarkPharaoh where
  getAbilities (SpeakerForTheDarkPharaoh a) =
    extend1 a
      $ restricted a 1 (HasScenarioCount StrengthOfTheAbyss $ atLeast 3)
      $ forced
      $ EnemyDefeated #after Anyone ByAny (be a)

instance RunMessage SpeakerForTheDarkPharaoh where
  runMessage msg e@(SpeakerForTheDarkPharaoh attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      withLocationOf attrs \loc -> do
        selectEach (investigatorAt loc) \iid -> assignDamage iid (attrs.ability 1) 1
      pure e
    _ -> SpeakerForTheDarkPharaoh <$> liftRunMessage msg attrs
