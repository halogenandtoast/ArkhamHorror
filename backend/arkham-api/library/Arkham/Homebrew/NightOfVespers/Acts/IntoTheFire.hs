module Arkham.Homebrew.NightOfVespers.Acts.IntoTheFire (intoTheFire) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.ChaosToken (ChaosTokenFace (..))
import Arkham.Homebrew.NightOfVespers.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.NightOfVespers.Helpers (scenarioI18n)
import Arkham.Homebrew.NightOfVespers.Jinx
import Arkham.I18n
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose

newtype IntoTheFire = IntoTheFire ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- The clue requirement is declared so the card prints it, but the advance is not
-- the engine's own free objective (ability 999, available during any
-- investigator's turn): this act only lets the clues be spent once every
-- undefeated investigator stands at one of the three escape routes.
intoTheFire :: ActCard IntoTheFire
intoTheFire =
  act (3, A) IntoTheFire Cards.intoTheFire (Just $ GroupClueCost (PerPlayer 2) Anywhere)

escapeRouteLocations :: LocationMatcher
escapeRouteLocations =
  mapOneOf locationIs [Locations.conservatory, Locations.hedgeMaze, Locations.hotelRooftop]

-- The side guard and the removal's window both matter: a bare list drops the
-- attrs' guard along with ability 999, and the removal takes the after-enter
-- window, which resolves before the after-move window the locations add on.
instance HasAbilities IntoTheFire where
  getAbilities (IntoTheFire a) =
    guard (onSide A a)
      *> [ mkAbility a 1 $ forced $ Enters #after You Anywhere
         , scenarioI18n "theEndOfSeasons" $ jinxAbilityI "intoTheFire.jinx" a 2
         , restricted
             a
             3
             (DuringTurn Anyone <> EachUndefeatedInvestigator (at_ escapeRouteLocations))
             $ Objective
             $ FastAbility
             $ GroupClueCost (PerPlayer 2) Anywhere
         ]

instance RunMessage IntoTheFire where
  runMessage msg a@(IntoTheFire attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach UnrevealedLocation removeLocation
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (jinxContext -> Just context) _ -> do
      resolveJinx attrs 2 context do
        let iid = context.investigator
        nearest <- select $ NearestEnemyTo iid ExhaustedEnemy
        chooseOrRunOneM iid $ withI18n do
          countVar 1 $ labeled "placeCluesOnYourLocation" $ placeCluesOnLocation iid (attrs.ability 2) 1
          unless (null nearest) $ labeled "readyEnemy" $ chooseTargetM iid nearest readyThis
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      push $ AdvanceAct attrs.id (InvestigatorSource iid) AdvancedWithClues
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      occupied <- select $ escapeRouteLocations <> LocationWithInvestigator Anyone
      chooseOrRunOneM lead $ targets occupied \lid -> do
        conservatory <- lid <=~> locationIs Locations.conservatory
        hedgeMaze <- lid <=~> locationIs Locations.hedgeMaze
        addChaosTokenForGame
          $ if conservatory then Tablet else if hedgeMaze then Cultist else ElderThing
      push R1
      pure a
    _ -> IntoTheFire <$> liftRunMessage msg attrs
