module Arkham.Enemy.Cards.MalevolentSpirit (malevolentSpirit) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Enemy
import Arkham.Helpers.Modifiers
import Arkham.Keyword (Keyword (Hunter))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Spectral))

newtype MalevolentSpirit = MalevolentSpirit EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

malevolentSpirit :: EnemyCard MalevolentSpirit
malevolentSpirit =
  enemy MalevolentSpirit Cards.malevolentSpirit (2, Static 2, 4) (0, 1)
    & setSpawnAt (mapOneOf LocationWithTitle ["Chapel Attic", "Chapel Crypt"])

instance HasModifiersFor MalevolentSpirit where
  getModifiersFor (MalevolentSpirit a) = do
    atSpectral <- selectAny $ locationWithEnemy (toId a) <> LocationWithTrait Spectral
    modifySelfWhen a atSpectral [AddKeyword Hunter, DamageDealt 1, HorrorDealt 1]

instance HasAbilities MalevolentSpirit where
  getAbilities (MalevolentSpirit a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyDefeated #when Anyone (BySource $ NotSource $ SourceMatchesAny [#spell, #relic]) (be a)

instance RunMessage MalevolentSpirit where
  runMessage msg e@(MalevolentSpirit attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      cancelEnemyDefeatWithWindows attrs.id
      healAllDamage (attrs.ability 1) attrs
      disengageEnemyFromAll attrs
      exhaustThis attrs
      spectralLocations <- select $ LocationWithTrait Spectral
      leadChooseOrRunOneM $ targets spectralLocations $ enemyMoveTo (attrs.ability 1) attrs
      pure e
    _ -> MalevolentSpirit <$> liftRunMessage msg attrs
