module Arkham.Homebrew.TheSymphonyOfErichZann.Acts.ThePossessedConductor (thePossessedConductor) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Locations
import Arkham.Card.CardDef (CardDef)
import Arkham.Matcher
import Arkham.Placement (Placement (AttachedToLocation))

newtype ThePossessedConductor = ThePossessedConductor ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePossessedConductor :: ActCard ThePossessedConductor
thePossessedConductor = act (2, A) ThePossessedConductor Cards.thePossessedConductor Nothing

-- | The six Backstage Rooms; four are put into play and two are removed.
backstageRooms :: [CardDef]
backstageRooms =
  [ Locations.anechoicChamber
  , Locations.instrumentCloset
  , Locations.recordingStudio
  , Locations.rehearsalRoom
  , Locations.sceneShop
  , Locations.tiringRoom
  ]

musicians :: [CardDef]
musicians =
  [ Enemies.arnoldWalker
  , Enemies.isabelLaFratta
  , Enemies.nicolePage
  , Enemies.songYin
  ]

instance HasAbilities ThePossessedConductor where
  getAbilities (ThePossessedConductor a) =
    [mkAbility a 1 $ Objective $ forced $ EnemyDefeated #after Anyone ByAny (enemyIs Enemies.augusteGaudinConductorOfTheVoid)]

instance RunMessage ThePossessedConductor where
  runMessage msg a@(ThePossessedConductor attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- "Put the set aside Stage Hall location and four random Backstage Room
      -- locations that were set aside into play. Remove the other two from the game."
      void $ placeLocationCard Locations.stageHall
      (kept, removed) <- splitAt 4 <$> shuffleM backstageRooms
      rooms <- traverse placeLocationCard kept
      traverse_ removeCardFromGame =<< fetchCards removed

      -- "Randomly spawn the four set aside Musician enemies, one on each
      -- Backstage Room location, enemy side face up."
      playingIsabel <- selectAny (InvestigatorWithTitle "Isabel La Fratta")
      shuffledMusicians <- shuffleM musicians
      for_ (zip rooms shuffledMusicians) \(room, musician) ->
        -- "If an investigator is playing Isabel La Fratta, remove the Isabel La
        -- Fratta enemy and attach the set-aside The Piano story asset at its
        -- location instead."
        if playingIsabel && musician == Enemies.isabelLaFratta
          then do
            removeCardFromGame =<< fetchCard Enemies.isabelLaFratta
            void $ createAssetAt Assets.thePiano (AttachedToLocation room)
          else createEnemyAt_ musician room

      -- "Set the Auguste Gaudin (Conductor of the Void) enemy aside, out of play
      -- and attach the set aside Auguste Gaudin (Maestro of Symphonies) story
      -- asset to the Stage Hall location."
      stageHall <- selectJust $ locationIs Locations.stageHall
      selectEach (enemyIs Enemies.augusteGaudinConductorOfTheVoid) \eid ->
        push $ RemoveFromPlay (toSource eid)
      void $ createAssetAt Assets.augusteGaudinMaestroOfSymphonies (AttachedToLocation stageHall)

      shuffleEncounterDiscardBackIn
      advanceActDeck attrs
      pure a
    _ -> ThePossessedConductor <$> liftRunMessage msg attrs
