{- | Shared rules for The Fall of the House of Usher.

The scenario's spine is damage on locations. Every [[Decadent]] room prints
"Forced - If there is damage on <room> equal to its shroud value: Flip <room>",
which swaps it for its [[Ruined]] face in place, and agenda 2 is what feeds that
damage in. Damage is never placed past a room's shroud, so a room collapses
exactly once.
-}
module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers where

import Arkham.Ability
import Arkham.Calculation
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Traits
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorName))
import Arkham.Location.Types (Field (..), LocationAttrs)
import Arkham.Matcher
import Arkham.Message (Message (ReplaceLocation), ReplaceStrategy (..))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Log (remember)
import Arkham.Name (Labeled (..), Named)
import Arkham.Name qualified as Name
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (ScenarioRemembered))
import Arkham.ScenarioLogKey (ScenarioLogKey (..))
import Arkham.Target (toTarget)
import Arkham.Trait (Trait (Ruined))

decadentLocation :: LocationMatcher
decadentLocation = LocationWithTrait Decadent

ruinedLocation :: LocationMatcher
ruinedLocation = LocationWithTrait Ruined

-- | How much more damage this room can take before it collapses.
roomCapacity :: HasGame m => LocationId -> m Int
roomCapacity lid = do
  shroud <- fromMaybe 0 <$> field LocationShroud lid
  damage <- field LocationDamage lid
  pure $ max 0 (shroud - damage)

{- | The rooms a point of "place N damage among Decadent locations in play, up
to their shroud values" may still go on. The caller places one point per
deferred step, so a room that fills up drops out for the remaining points.
-}
decadentRoomsWithCapacity :: HasGame m => m [LocationId]
decadentRoomsWithCapacity = filterM (fmap (> 0) . roomCapacity) =<< select decadentLocation

{- | "Forced - If there is damage on <room> equal to its shroud value: Flip
<room>." Hung off the damage placement rather than @AnyWindow@: the modified
shroud dips while a Flashlight-style reduction is live, and an @AnyWindow@
Forced would read that dip as a collapse. @onlyOnce@ is what keeps it from
re-firing before 'flipToRuined's swap resolves.
-}
collapseForced :: HasI18n => Text -> LocationAttrs -> Int -> Ability
collapseForced tip a n =
  withI18nTooltip tip
    $ onlyOnce
    $ restricted a n damageAtShroud
    $ forced (PlacedToken #after AnySource (TargetIs $ toTarget a.id) #damage)
 where
  damageAtShroud =
    HasCalculation
      ( SubtractCalculation
          (LocationFieldCalculation a.id LocationDamage)
          (LocationFieldCalculation a.id LocationJustShroud)
      )
      (AtLeast $ Static 0)

{- | "This ability cannot be activated if a copy of The Mad Trist has already
entered play during this scenario." Both copies start set aside and the two
Ruined rooms are the only things that put one into play, so "still set aside" is
exactly "has not entered play".
-}
noMadTristHasEnteredPlay :: Criterion
noMadTristHasEnteredPlay =
  Criteria
    [ SetAsideCardExists (cardIs Assets.theMadTristDragonsCunning)
    , SetAsideCardExists (cardIs Assets.theMadTristShelterFromTheStorm)
    ]

-- | Swap a collapsed [[Decadent]] room for its [[Ruined]] face, in place.
flipToRuined :: ReverseQueue m => LocationAttrs -> m ()
flipToRuined attrs =
  for_ (toCardDef attrs).otherSide \other ->
    push $ ReplaceLocation attrs.id (lookupCard other attrs.cardId) Swap

{- | The Private Library remembers this once for the whole group (it prints a
group limit), and both Ruined Mad Trist locations read it back.
-}
unearthedAnArchaicBook :: ScenarioLogKey
unearthedAnArchaicBook =
  HomebrewScenarioLogKey "theFallOfTheHouseOfUsher.UnearthedAnArchaicBook"

{- | Each investigator pays their own damage and horror for this at the Banquet
Hall, so it is remembered about them individually.
-}
acceptedTheUshersHospitalityKey :: Text
acceptedTheUshersHospitalityKey = "theFallOfTheHouseOfUsher.AcceptedTheUshersHospitality"

acceptedTheUshersHospitalityFor :: Named name => name -> InvestigatorId -> ScenarioLogKey
acceptedTheUshersHospitalityFor name iid =
  HomebrewScenarioLogKeyFor acceptedTheUshersHospitalityKey (Name.labeled name iid)

recordHospitality :: ReverseQueue m => InvestigatorId -> m ()
recordHospitality iid = do
  name <- field InvestigatorName iid
  remember $ acceptedTheUshersHospitalityFor name iid

{- | The criterion form. The scenario republishes the log read as a
'ScenarioModifier' so a pure @getAbilities@ can name it; 'hasAcceptedHospitality'
is the monadic form for everywhere else.
-}
investigatorWithHospitality :: InvestigatorMatcher
investigatorWithHospitality =
  InvestigatorWithModifier $ ScenarioModifier acceptedTheUshersHospitalityKey

hasAcceptedHospitality :: HasGame m => InvestigatorId -> m Bool
hasAcceptedHospitality iid = do
  ks <- scenarioField ScenarioRemembered
  pure $ any isHospitalityFor (toList ks)
 where
  isHospitalityFor = \case
    HomebrewScenarioLogKeyFor t (Labeled _ i) -> t == acceptedTheUshersHospitalityKey && i == iid
    _ -> False

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theFallOfTheHouseOfUsher" a

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "scenario" a
