module Arkham.Agenda.Cards.SinkingGround (sinkingGround) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Bog))

newtype SinkingGround = SinkingGround AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sinkingGround :: AgendaCard SinkingGround
sinkingGround = agenda (1, A) SinkingGround Cards.sinkingGround (Static 3)

instance HasAbilities SinkingGround where
  getAbilities (SinkingGround a) =
    [ mkAbility a 1 $ ActionAbility #resign Nothing (ActionCost 1)
    , mkAbility a 2 $ forced $ TurnEnds #after (You <> at_ (LocationWithTrait Bog))
    , mkAbility a 3
        $ forced
        $ PlacedToken #after AnySource (LocationTargetMatches $ LocationWithDamage (atLeast 3)) #damage
    ]

instance RunMessage SinkingGround where
  runMessage msg a@(SinkingGround attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      playerCount <- getPlayerCount
      let sinkholes = if playerCount <= 2 then 2 else 1
      withLocationOf iid \lid ->
        placeTokens (attrs.ability 2) lid #damage sinkholes
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      lids <- select $ LocationWithDamage (atLeast 3)
      for_ lids \lid -> do
        removeAllOfTokenOn attrs Token.Damage lid
        lead <- getLead
        flipOverBy lead attrs lid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      centerLid <- selectJust $ LocationInPosition (Pos 0 0)
      push $ RemoveAllTokens (toSource attrs) (toTarget centerLid)
      selectEach (AssetAttachedTo (TargetIs $ toTarget centerLid)) (toDiscard attrs)
      selectEach (EventAttachedTo (TargetIs $ toTarget centerLid)) (toDiscard attrs)
      lead <- getLead
      flipOverBy lead attrs centerLid
      spawnEnemyAt_ Enemies.thingInTheDepths centerLid
      advanceAgendaDeck attrs
      pure a
    _ -> SinkingGround <$> liftRunMessage msg attrs
