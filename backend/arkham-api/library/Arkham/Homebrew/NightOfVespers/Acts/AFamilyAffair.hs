module Arkham.Homebrew.NightOfVespers.Acts.AFamilyAffair (aFamilyAffair) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Creation (createExhausted)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Homebrew.NightOfVespers.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.NightOfVespers.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.NightOfVespers.Sets qualified as Set
import Arkham.Location.Types qualified as Location
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection

newtype AFamilyAffair = AFamilyAffair ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- The clue requirement is declared so the card prints it, but the advance is not
-- the engine's own free objective (ability 999, available during any
-- investigator's turn): this act only lets the clues be spent once every
-- undefeated investigator stands in the Grand Ballroom.
aFamilyAffair :: ActCard AFamilyAffair
aFamilyAffair =
  act (2, A) AFamilyAffair Cards.aFamilyAffair (Just $ GroupClueCost (PerPlayer 2) Anywhere)

-- The side guard and the removal's window both matter: a bare list drops the
-- attrs' guard along with ability 999, and this act's own advance body moves
-- investigators while it is still in play. The removal takes the after-enter
-- window, which resolves before the after-move window the locations add on.
instance HasAbilities AFamilyAffair where
  getAbilities (AFamilyAffair a) =
    guard (onSide A a)
      *> [ mkAbility a 1 $ forced $ Enters #after You Anywhere
         , restricted
             a
             2
             ( DuringTurn Anyone
                 <> EachUndefeatedInvestigator (at_ $ locationIs Locations.grandBallroom)
             )
             $ Objective
             $ FastAbility
             $ GroupClueCost (PerPlayer 2) Anywhere
         ]

instance RunMessage AFamilyAffair where
  runMessage msg a@(AFamilyAffair attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach UnrevealedLocation \lid -> do
        card <- field Location.LocationCard lid
        removeLocationWithoutVictory lid
        setAsideCards [toCardDef card]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ AdvanceAct attrs.id (InvestigatorSource iid) AdvancedWithClues
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (UnengagedEnemy <> EnemyAt Anywhere) $ toDiscard attrs
      trattoria <- selectJust $ LocationWithTitle "Trattoria del Parche"
      eachInvestigator \iid -> moveTo attrs iid trattoria
      lead <- getLead
      returning <-
        getSetAsideCardsMatching
          $ #location
          <> mapOneOf
            cardIs
            [ Locations.barLAmmaliatrice_013
            , Locations.barLAmmaliatrice_014
            , Locations.trattoriaKitchens_017
            , Locations.trattoriaKitchens_018
            ]
      chooseOrRunOneM lead $ targets returning placeLocation_
      selectEach (RevealedLocation <> not_ (LocationWithTitle "Trattoria del Parche")) removeLocation
      placeCluesUpToClueValue attrs trattoria
      createSetAsideEnemyWith_ Enemies.declanODoyle trattoria createExhausted
      shuffleSetAsideEncounterSet Set.Inferno
      shuffleEncounterDiscardBackIn
      advanceActDeck attrs
      pure a
    _ -> AFamilyAffair <$> liftRunMessage msg attrs
