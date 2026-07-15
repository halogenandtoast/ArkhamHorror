module Arkham.Homebrew.DarkMatter.Enemies.StalkingByakhee (stalkingByakhee) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype StalkingByakhee = StalkingByakhee EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkingByakhee :: EnemyCard StalkingByakhee
stalkingByakhee =
  enemy StalkingByakhee Cards.stalkingByakhee
    & setSpawnAt (LocationWithTitle "Entrance Tunnel")

instance HasModifiersFor StalkingByakhee where
  getModifiersFor (StalkingByakhee a) =
    modifySelf a [AddKeyword Keyword.Hunter, AddKeyword Keyword.Alert]

instance HasAbilities StalkingByakhee where
  getAbilities (StalkingByakhee a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage StalkingByakhee where
  runMessage msg e@(StalkingByakhee attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connected <- select $ connectedFrom (locationWithEnemy attrs)
      unless (null connected) do
        counts <- for connected \lid -> (lid,) <$> selectCount (InvestigatorAt (LocationWithId lid))
        for_ (fromNullable $ map snd counts) \cs -> do
          let fewest = minimum cs
              destinations = [lid | (lid, c) <- counts, c == fewest]
          chooseOrRunOneM iid $ targets destinations (enemyMoveTo (attrs.ability 1) attrs)
      pure e
    _ -> StalkingByakhee <$> liftRunMessage msg attrs
