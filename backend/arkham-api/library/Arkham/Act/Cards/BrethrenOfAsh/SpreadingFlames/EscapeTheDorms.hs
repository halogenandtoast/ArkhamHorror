module Arkham.Act.Cards.BrethrenOfAsh.SpreadingFlames.EscapeTheDorms (escapeTheDorms) where

import Arkham.Ability
import Arkham.Act.CardDefs.BrethrenOfAsh.SpreadingFlames qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.CardDefs.BrethrenOfAsh.SpreadingFlames qualified as Enemies
import Arkham.Location.CardDefs.BrethrenOfAsh.MiskatonicUniversity qualified as Locations
import Arkham.Location.CardDefs.BrethrenOfAsh.SpreadingFlames qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype EscapeTheDorms = EscapeTheDorms ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapeTheDorms :: ActCard EscapeTheDorms
escapeTheDorms = act (2, A) EscapeTheDorms Cards.escapeTheDorms Nothing

instance HasAbilities EscapeTheDorms where
  getAbilities = actAbilities \a ->
    [ restricted
        a
        1
        (EachUndefeatedInvestigator $ at_ $ locationIs Locations.miskatonicQuad)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage EscapeTheDorms where
  runMessage msg a@(EscapeTheDorms attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectOne (IncludeOutOfPlayEnemy $ EnemyWithTitle "Servant of Flame") >>= \case
        Just servant -> do
          healAllDamage attrs servant
          obtainCard =<< fetchCard Enemies.servantOfFlameRagingFury
          place servant SetAsideZone
        Nothing -> do
          card <- fetchCard Enemies.servantOfFlameRagingFury
          obtainCard card
          createEnemyWithM_ card (OutOfPlay SetAsideZone) \c ->
            setAfter c (healAllDamage attrs c.enemy)

      doStep 1 msg

      selectEach (locationIs Locations.yourFriendsRoom) removeLocation

      placeSetAsideLocations_
        [ Locations.orneLibrary
        , Locations.scienceHall
        , Locations.warrenObservatory
        ]

      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      discardEach attrs (AnyEnemy)
      pure a
    _ -> EscapeTheDorms <$> liftRunMessage msg attrs
