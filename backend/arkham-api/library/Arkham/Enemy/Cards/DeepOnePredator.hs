module Arkham.Enemy.Cards.DeepOnePredator (deepOnePredator, DeepOnePredator (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated, EnemyEvaded)
import Arkham.Investigator.Projection ()
import Arkham.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DeepOnePredator = DeepOnePredator EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOnePredator :: EnemyCard DeepOnePredator
deepOnePredator = enemy DeepOnePredator Cards.deepOnePredator (4, Static 2, 2) (0, 1)

instance HasAbilities DeepOnePredator where
  getAbilities (DeepOnePredator a) =
    extend
      a
      [ forcedAbility a 1 $ EnemyEngaged #after You (be a)
      , restricted a 2 criteria
          $ forced
          $ oneOf [EnemyDefeated #when You ByAny (be a), EnemyEvaded #when You (be a)]
      ]
   where
    criteria = if null a.keys && a.token #clue == 0 then Never else NoRestriction

instance RunMessage DeepOnePredator where
  runMessage msg e@(DeepOnePredator attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ks <- iid.keys
      clues <- iid.clues
      chooseOneM iid do
        for_ ks \k ->
          labeled ("Move " <> keyName k <> " key to Deep One Predator") $ placeKey attrs k
        when (clues > 0) do
          labeled "Move one of your clues to Deep One Predator" do
            moveTokens (attrs.ability 1) iid attrs #clue 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      for_ attrs.keys $ placeKey iid
      when (attrs.token #clue > 0) do
        moveTokens (attrs.ability 2) attrs iid #clue (attrs.token #clue)
      pure e
    _ -> DeepOnePredator <$> liftRunMessage msg attrs
