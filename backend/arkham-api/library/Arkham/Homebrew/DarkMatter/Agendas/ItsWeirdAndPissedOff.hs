module Arkham.Homebrew.DarkMatter.Agendas.ItsWeirdAndPissedOff (itsWeirdAndPissedOff) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Window (assetLeavingPlay)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.Helpers (scanTopOfScanningDeck, scenarioI18n)
import Arkham.Homebrew.DarkMatter.MotionScanning
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (enemyMoveTo)
import Arkham.Placement

newtype ItsWeirdAndPissedOff = ItsWeirdAndPissedOff AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

itsWeirdAndPissedOff :: AgendaCard ItsWeirdAndPissedOff
itsWeirdAndPissedOff = agenda (4, A) ItsWeirdAndPissedOff Cards.itsWeirdAndPissedOff (Static 2)

instance HasModifiersFor ItsWeirdAndPissedOff where
  getModifiersFor (ItsWeirdAndPissedOff a) = motionScanModifiers a

instance HasAbilities ItsWeirdAndPissedOff where
  getAbilities (ItsWeirdAndPissedOff a) = motionScanAbilities a

instance RunMessage ItsWeirdAndPissedOff where
  runMessage msg a@(ItsWeirdAndPissedOff attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scanTopOfScanningDeck iid (attrs.ability 1)
      pure a
    {- Only the final agenda keeps the crew instead of removing them: "Forced -
    When a [[Crew]] story asset is defeated or discarded: Attach it facedown to
    the Entity instead." The asset is still in play here, so it survives the move
    as an entity — act 2b's version, which attaches crew that are only cards,
    has to place them underneath instead. -}
    UseCardAbility _ (isSource attrs -> True) 2 (assetLeavingPlay -> aid) _ -> do
      selectOne (enemyIs Enemies.theEntity)
        >>= traverse_ (push . PlaceAsset aid . AttachedToEnemy)
      pure a
    {- Agenda 4b:

    "The Entity immediately moves to the nearest investigator's location and
    attacks each investigator at that location.
    Flip this agenda back to agenda 4a."

    The scenario never ends on this agenda ("Hint: The scenario will not end when
    this agenda advances"), so it always reverts. The attack has to wait for the
    move to land, so it is deferred behind the enemy rather than queued
    alongside it — a blocked move would otherwise attack at the old location. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      scenarioI18n "inTheShadowOfEarth" $ scope "agenda4b" do
        flavor $ setTitle "title" >> p "body"
      selectOne (enemyIs Enemies.theEntity) >>= traverse_ \eid ->
        withLocationOf eid \from -> do
          dests <- select $ NearestLocationToLocation from (LocationWithInvestigator Anyone)
          lead <- getLead
          chooseOrRunOneM lead $ targets dests \dest -> do
            enemyMoveTo attrs eid dest
            forTarget_ eid msg
      revertAgenda attrs
      pure a
    ForTarget (EnemyTarget eid) (AdvanceAgenda (isSide B attrs -> True)) -> do
      withLocationOf eid \lid ->
        selectEach (investigatorAt lid) (initiateEnemyAttack eid attrs)
      pure a
    _ -> ItsWeirdAndPissedOff <$> liftRunMessage msg attrs
