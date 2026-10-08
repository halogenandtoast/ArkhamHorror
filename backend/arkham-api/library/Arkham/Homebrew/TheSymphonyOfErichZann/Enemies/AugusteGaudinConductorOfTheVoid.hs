module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.AugusteGaudinConductorOfTheVoid (augusteGaudinConductorOfTheVoid) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.GameEnv (getCard)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (
  getMusicOrder,
  musicTreacheriesInPlay,
  setMusicOrder,
 )
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Music)
import Arkham.Matcher
import Arkham.Message qualified as Msg

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
      [ -- Cannot be defeated while a Music treachery is in play; discards one instead.
        restricted a 1 (exists $ TreacheryWithTrait Music <> InPlayTreachery)
          $ forced
          $ EnemyWouldBeDefeated #when (be a)
      , -- Parley for 2 damage, paid with a clue.
        restricted a 2 OnSameLocation $ parleyAction (ClueCost $ Static 1)
      , {- Act 2 advances on his defeat and says to "set the Auguste Gaudin
        (Conductor of the Void) enemy aside, out of play", but by then he has
        been discarded and the same act shuffles the encounter discard back in
        -- which would deal him out as an encounter card and leave Beyond the
        Curtain with nothing to spawn. Redirect the card as he leaves play. -}
        mkAbility a 3 $ forced $ EnemyLeavesPlay #when (be a)
      ]

instance RunMessage AugusteGaudinConductorOfTheVoid where
  runMessage msg e@(AugusteGaudinConductorOfTheVoid attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      music <- musicTreacheriesInPlay
      for_ (headMay music) \tid -> do
        toDiscard (attrs.ability 1) tid
        setMusicOrder . filter (/= tid) =<< getMusicOrder
      cancelEnemyDefeat attrs
      healAllDamage (attrs.ability 1) attrs
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 2) 2 attrs
      pure e
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      -- He still leaves play; only the card's destination is replaced. Dropping
      -- the queued discard is what keeps it out of the encounter discard so
      -- `SetCardAside` is the only place it lands.
      card <- getCard attrs.cardId
      allMatchingDon't \case
        Msg.Discarded target _ _ -> isTarget attrs target
        Do (Msg.Discarded target _ _) -> isTarget attrs target
        _ -> False
      push $ SetCardAside card
      -- The entity outlives the card, so the damage that defeated him would
      -- still be showing when Beyond the Curtain spawns him again.
      pure $ AugusteGaudinConductorOfTheVoid (attrs & tokensL .~ mempty)
    _ -> AugusteGaudinConductorOfTheVoid <$> liftRunMessage msg attrs
