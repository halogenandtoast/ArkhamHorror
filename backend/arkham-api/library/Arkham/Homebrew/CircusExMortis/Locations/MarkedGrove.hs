module Arkham.Homebrew.CircusExMortis.Locations.MarkedGrove (markedGrove) where

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

{- | The back of the Scribe the Sigil Destiny story (:205b). 'otherSideIs' makes the def
single-sided, so it enters play already revealed; its printed clue value is 0 and fixed,
and the horror on it is what the investigators have to pile up.
-}
newtype MarkedGrove = MarkedGrove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markedGrove :: LocationCard MarkedGrove
markedGrove = location MarkedGrove Cards.markedGrove 2 (Static 0)

{- | 1. "[action]: Test [willpower] or [intellect] (4). If you succeed, place 1 horror on
Marked Grove."
2. "If there is 3 horror on Marked Grove, investigators whose destiny is not \"sigil\"
cannot activate the above ability." A restriction on the printed action, not a second
ability, so it is a criterion on ability 1 -- which also keeps the engine from offering
the action to a seat that cannot take it. 'investigatorWithDestinyModifier' is the pure
form of the destiny lookup; the scenario republishes each dealt destiny as a
'ScenarioModifier' precisely because 'getAbilities' cannot read the campaign log.
3. "Forced - If there is 4 horror on Marked Grove: Move each investigator and enemy on it to
a connecting location, flip it, and move it to the victory display." 'AnyWindow' is safe
here, unlike on Forest Chasm: the counter starts at 0 and only this card's own action ever
adds to it, so the condition cannot be true in the location's enters-play window.
-}
instance HasAbilities MarkedGrove where
  getAbilities (MarkedGrove a) =
    extendRevealed
      a
      [ skillTestAbility $ restricted a 1 (Here <> canWork) actionAbility
      , onlyOnce $ restricted a 2 (thisExists a $ LocationWithHorror $ atLeast 4) $ forced AnyWindow
      ]
   where
    canWork =
      oneOf
        [ thisExists a $ LocationWithHorror (lessThan 3)
        , youExist (investigatorWithDestinyModifier "sigil")
        ]

instance RunMessage MarkedGrove where
  runMessage msg l@(MarkedGrove attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid $ skillsLabeled [#willpower, #intellect] \sk ->
        beginSkillTest sid iid (attrs.ability 1) attrs sk (Fixed 4)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeTokens (attrs.ability 1) attrs #horror 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      destinyLocationOvercome (attrs.ability 2) iid Stories.scribeTheSigil attrs
      pure l
    _ -> MarkedGrove <$> liftRunMessage msg attrs
