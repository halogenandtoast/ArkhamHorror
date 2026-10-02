module Arkham.Homebrew.CircusExMortis.Locations.CanyonEntrance (canyonEntrance) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.Helpers (
  destinyLocationOvercome,
  investigatorWithDestinyModifier,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

{- | The back of the Split the Rock Destiny story (:204b). 'otherSideIs' makes the def
single-sided, so it enters play already revealed; its printed clue value is 0 and fixed,
and the damage on it is what the investigators have to pile up.
-}
newtype CanyonEntrance = CanyonEntrance LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

canyonEntrance :: LocationCard CanyonEntrance
canyonEntrance = location CanyonEntrance Cards.canyonEntrance 2 (Static 0)

{- | 1. "[action]: Test [combat] or [agility] (4). If you succeed, place 1 damage on
Canyon Entrance."
2. "If there is 3 damage on Canyon Entrance, investigators whose destiny is not \"rock\"
cannot activate the above ability." A restriction on the printed action, not a second
ability, so it is a criterion on ability 1 -- which also keeps the engine from offering
the action to a seat that cannot take it. 'investigatorWithDestinyModifier' is the pure
form of the destiny lookup; the scenario republishes each dealt destiny as a
'ScenarioModifier' precisely because 'getAbilities' cannot read the campaign log.
3. "Forced - If there is 4 damage on Canyon Entrance: Move each investigator and enemy on it to
a connecting location, flip it, and move it to the victory display." 'AnyWindow' is safe
here, unlike on Forest Chasm: the counter starts at 0 and only this card's own action ever
adds to it, so the condition cannot be true in the location's enters-play window.
-}
instance HasAbilities CanyonEntrance where
  getAbilities (CanyonEntrance a) =
    extendRevealed
      a
      [ skillTestAbility $ restricted a 1 (Here <> canWork) actionAbility
      , onlyOnce $ restricted a 2 (thisExists a $ LocationWithDamage $ atLeast 4) $ forced AnyWindow
      ]
   where
    canWork =
      oneOf
        [ thisExists a $ LocationWithDamage (lessThan 3)
        , youExist (investigatorWithDestinyModifier "rock")
        ]

instance RunMessage CanyonEntrance where
  runMessage msg l@(CanyonEntrance attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid $ skillsLabeled [#combat, #agility] \sk ->
        beginSkillTest sid iid (attrs.ability 1) attrs sk (Fixed 4)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeTokens (attrs.ability 1) attrs #damage 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      destinyLocationOvercome (attrs.ability 2) iid Stories.splitTheRock attrs
      pure l
    _ -> CanyonEntrance <$> liftRunMessage msg attrs
