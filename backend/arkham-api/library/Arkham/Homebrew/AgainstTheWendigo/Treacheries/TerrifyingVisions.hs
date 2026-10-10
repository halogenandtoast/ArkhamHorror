module Arkham.Homebrew.AgainstTheWendigo.Treacheries.TerrifyingVisions (terrifyingVisions) where

import Arkham.Ability
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Animal)
import Arkham.Matcher
import Arkham.Trait (Trait (Monster))
import Arkham.Treachery.Import.Lifted

newtype TerrifyingVisions = TerrifyingVisions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrifyingVisions :: TreacheryCard TerrifyingVisions
terrifyingVisions = treachery TerrifyingVisions Cards.terrifyingVisions

instance HasAbilities TerrifyingVisions where
  getAbilities (TerrifyingVisions a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ TurnBegins #when You]

instance RunMessage TerrifyingVisions where
  runMessage msg t@(TerrifyingVisions attrs) = runQueueT $ case msg of
    -- "Revelation - Test {willpower} (4). If you fail, put Terrifying Visions
    -- into your threat area."
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    -- "Forced - At the beginning of your turn, take 1 horror for each Animal
    -- enemy and each Monster enemy in your location."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- selectCount $ enemyAtLocationWith iid <> mapOneOf EnemyWithTrait [Animal, Monster]
      assignHorror iid (attrs.ability 1) n
      pure t
    _ -> TerrifyingVisions <$> liftRunMessage msg attrs
