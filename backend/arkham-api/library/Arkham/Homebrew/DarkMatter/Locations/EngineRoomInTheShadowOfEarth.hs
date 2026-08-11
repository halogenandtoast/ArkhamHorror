module Arkham.Homebrew.DarkMatter.Locations.EngineRoomInTheShadowOfEarth (
  engineRoomInTheShadowOfEarth,
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype EngineRoomInTheShadowOfEarth = EngineRoomInTheShadowOfEarth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

engineRoomInTheShadowOfEarth :: LocationCard EngineRoomInTheShadowOfEarth
engineRoomInTheShadowOfEarth =
  location EngineRoomInTheShadowOfEarth Cards.engineRoomInTheShadowOfEarth 3 (PerPlayer 1)

{- | "[action] Deal 2 damage to each investigator and enemy at this location:
Gain 2 clues from the token bank. (Limit once per round.)"
-}
instance HasAbilities EngineRoomInTheShadowOfEarth where
  getAbilities (EngineRoomInTheShadowOfEarth a) =
    extendRevealed1 a $ playerLimit PerRound $ restricted a 1 Here actionAbility

instance RunMessage EngineRoomInTheShadowOfEarth where
  runMessage msg l@(EngineRoomInTheShadowOfEarth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      here <- select $ investigatorAt attrs.id
      for_ here \iid' -> assignDamage iid' (attrs.ability 1) 2
      enemies <- select $ enemyAt attrs.id
      for_ enemies \enemy -> nonAttackEnemyDamage Nothing (attrs.ability 1) 2 enemy
      gainClues iid (attrs.ability 1) 2
      pure l
    _ -> EngineRoomInTheShadowOfEarth <$> liftRunMessage msg attrs
