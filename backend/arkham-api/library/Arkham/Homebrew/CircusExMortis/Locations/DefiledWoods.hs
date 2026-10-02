module Arkham.Homebrew.CircusExMortis.Locations.DefiledWoods (defiledWoods) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.Helpers (
  destinyLocationOvercome,
  investigatorWithDestinyModifier,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (setTokens)

{- | The back of the Cleanse the Stain Destiny story (:206b).

Cleanse the Stain says "put it into play with 6 resources on it", and those 6 are stocked
here, in the builder, rather than pushed by the story. 'PlacedLocation' opens the
enters-play windows *ahead* of anything the story queues behind the placement, and the
Forced below reads "if there are no resources on" this location -- so a story-side push
would hand that window a location with 0 resources on it and the woods would clear
themselves on arrival.
-}
newtype DefiledWoods = DefiledWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

defiledWoods :: LocationCard DefiledWoods
defiledWoods =
  locationWith DefiledWoods Cards.defiledWoods 4 (Static 0) (tokensL %~ setTokens #resource 6)

{- | 1. "[action] Choose and discard cards from your hand with 2 or more total skill icons:
Remove 1 resource from Defiled Woods." 'SkillIconCost' with no icon set counts every icon,
and it is checked when the action is offered, so the ability never appears to a hand that
cannot pay it.
2. "If there is exactly 1 resource on Defiled Woods, investigators whose destiny is not
\"stain\" cannot activate the above ability." A restriction on the printed action rather
than a second ability, so it rides as a criterion -- 2 or more resources is open to
everybody, the last one only to the "stain" seat.
3. "Forced - If there are no resources on Defiled Woods: Move each investigator and enemy
on it to a connecting location, flip it, and move it to the victory display."
-}
instance HasAbilities DefiledWoods where
  getAbilities (DefiledWoods a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> thisExists a (LocationWithResources $ atLeast 1) <> canPurify)
          $ actionAbilityWithCost (SkillIconCost 2 mempty)
      , onlyOnce $ restricted a 2 (thisExists a $ LocationWithResources $ atMost 0) $ forced AnyWindow
      ]
   where
    canPurify =
      oneOf
        [ thisExists a $ LocationWithResources (atLeast 2)
        , youExist (investigatorWithDestinyModifier "stain")
        ]

instance RunMessage DefiledWoods where
  runMessage msg l@(DefiledWoods attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeTokens (attrs.ability 1) attrs #resource 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      destinyLocationOvercome (attrs.ability 2) iid Stories.cleanseTheStain attrs
      pure l
    _ -> DefiledWoods <$> liftRunMessage msg attrs
