module Arkham.Enemy.Cards.InnsmouthShoggoth (innsmouthShoggoth, InnsmouthShoggoth (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers, modifySelf)
import Arkham.Id
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype Meta = Meta {barriers :: Maybe (LocationId, LocationId)}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype InnsmouthShoggoth = InnsmouthShoggoth (EnemyAttrs `With` Meta)
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthShoggoth :: EnemyCard InnsmouthShoggoth
innsmouthShoggoth = enemy (InnsmouthShoggoth . (`with` Meta Nothing)) Cards.innsmouthShoggoth (3, Static 4, 2) (2, 2)

instance HasModifiersFor InnsmouthShoggoth where
  getModifiersFor (InnsmouthShoggoth (With a _)) = do
    healthModifier <- perPlayer 2
    modifySelf a [HealthModifier healthModifier]

instance HasAbilities InnsmouthShoggoth where
  getAbilities (InnsmouthShoggoth (With a meta)) =
    extend1 a $ restricted a 1 criteria $ forced $ EnemyMovedTo #after Anywhere MovedViaAny (be a)
   where
    criteria = if isJust (barriers meta) then NoRestriction else Never

instance RunMessage InnsmouthShoggoth where
  runMessage msg e@(InnsmouthShoggoth (With attrs meta)) = runQueueT $ case msg of
    EnemyMove eid lid | eid == attrs.id -> do
      getLocationOf eid >>= \case
        Nothing -> InnsmouthShoggoth . (`with` meta) <$> liftRunMessage msg attrs
        Just lid' -> do
          attrs' <- liftRunMessage msg attrs
          mods <- getModifiers lid'
          let barriers = concat [ls | Barricades ls <- mods]
          pure . InnsmouthShoggoth $ attrs' `with` Meta (guard (lid `elem` barriers) $> (lid, lid'))
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ (barriers meta) \k -> push $ ScenarioCountSet (uncurry Barriers k) 0
      pure e
    _ -> InnsmouthShoggoth . (`with` meta) <$> liftRunMessage msg attrs
