module Arkham.Agenda.Cards.TheThingInTheBog (theThingInTheBog) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Matcher
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Bog))

newtype TheThingInTheBog = TheThingInTheBog AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThingInTheBog :: AgendaCard TheThingInTheBog
theThingInTheBog = agenda (2, A) TheThingInTheBog Cards.theThingInTheBog (Static 10)

instance HasAbilities TheThingInTheBog where
  getAbilities (TheThingInTheBog a) =
    [ mkAbility a 1 $ forced $ TurnEnds #after (You <> at_ (LocationWithTrait Bog))
    , mkAbility a 2
        $ forced
        $ PlacedToken #after AnySource (LocationTargetMatches $ LocationWithDamage (atLeast 3)) #damage
    , mkAbility a 3 $ Objective $ forced $ ifEnemyDefeated Enemies.thingInTheDepths
    ]

instance RunMessage TheThingInTheBog where
  runMessage msg a@(TheThingInTheBog attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      playerCount <- getPlayerCount
      let sinkholes = if playerCount <= 2 then 2 else 1
      withLocationOf iid \lid ->
        placeTokens (attrs.ability 1) lid #damage sinkholes
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      lids <- select $ LocationWithDamage (atLeast 3)
      for_ lids \lid -> do
        removeAllOfTokenOn attrs Token.Damage lid
        lead <- getLead
        flipOverBy lead attrs lid
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      push R4
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> TheThingInTheBog <$> liftRunMessage msg attrs
