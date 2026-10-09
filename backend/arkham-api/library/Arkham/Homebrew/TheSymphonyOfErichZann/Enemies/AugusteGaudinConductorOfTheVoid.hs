module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.AugusteGaudinConductorOfTheVoid (augusteGaudinConductorOfTheVoid) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (
  getMusicOrder,
  musicTreacheriesInPlay,
  setMusicOrder,
 )
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Music)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype AugusteGaudinConductorOfTheVoid = AugusteGaudinConductorOfTheVoid EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

augusteGaudinConductorOfTheVoid :: EnemyCard AugusteGaudinConductorOfTheVoid
augusteGaudinConductorOfTheVoid =
  enemy AugusteGaudinConductorOfTheVoid Cards.augusteGaudinConductorOfTheVoid
    & setSpawnAt (locationIs Locations.auditorium)

instance HasAbilities AugusteGaudinConductorOfTheVoid where
  getAbilities (AugusteGaudinConductorOfTheVoid a) =
    extend
      a
      [ restricted a 1 (exists $ TreacheryWithTrait Music <> InPlayTreachery)
          $ forced
          $ EnemyWouldBeDefeated #when (be a)
      , restricted a 2 OnSameLocation $ parleyAction (ClueCost $ Static 1)
      ]

instance RunMessage AugusteGaudinConductorOfTheVoid where
  runMessage msg e@(AugusteGaudinConductorOfTheVoid attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      music <- musicTreacheriesInPlay
      chooseTargetM iid music \tid -> do
        toDiscard (attrs.ability 1) tid
        setMusicOrder . filter (/= tid) =<< getMusicOrder
      cancelEnemyDefeat attrs
      healAllDamage (attrs.ability 1) attrs
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 2) 2 attrs
      pure e
    _ -> AugusteGaudinConductorOfTheVoid <$> liftRunMessage msg attrs
