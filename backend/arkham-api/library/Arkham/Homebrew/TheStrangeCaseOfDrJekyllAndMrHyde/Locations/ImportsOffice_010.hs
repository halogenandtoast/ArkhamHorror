{- | Imports Office (010) -- trade clues for a discount on an asset.

"Any amount of your clues" is an amount prompt rather than a cost, so the
reduction is known before the asset is chosen: the playable list is computed
under the same 'ReduceCostOf' the play will actually get, otherwise an asset
only affordable with the discount would never be offered.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.ImportsOffice_010 (importsOffice_010) where

import Arkham.Ability
import Arkham.Card (card_)
import Arkham.Helpers.Modifiers (ModifierType (..), withModifiers)
import Arkham.Helpers.Playable (getPlayableCardsMatch)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype ImportsOffice_010 = ImportsOffice_010 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

importsOffice_010 :: LocationCard ImportsOffice_010
importsOffice_010 = location ImportsOffice_010 Cards.importsOffice_010 2 (PerPlayer 2)

instance HasAbilities ImportsOffice_010 where
  getAbilities (ImportsOffice_010 a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "importsOffice.play"
      $ groupLimit PerRound
      $ restricted a 1 Here actionAbility

instance RunMessage ImportsOffice_010 where
  runMessage msg l@(ImportsOffice_010 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clues <- field InvestigatorClues iid
      scenarioI18n
        $ scope "importsOffice"
        $ countVar clues
        $ chooseAmount iid "cluesToPlace" "$clues" 0 clues attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) -> do
      placeCluesOnLocation iid (attrs.ability 1) n
      let x = 2 * n
      cards <-
        withModifiers iid (toModifiers (attrs.ability 1) [ReduceCostOf AnyCard x])
          $ getPlayableCardsMatch
            (attrs.ability 1)
            iid
            (UnpaidCost NoAction)
            (defaultWindows iid)
            (card_ #asset)
      unless (null cards) do
        chooseOneM iid $ targets cards \card -> do
          when (x > 0) $ reduceCostOf (attrs.ability 1) card x
          playCardPayingCost iid card
      pure l
    _ -> ImportsOffice_010 <$> liftRunMessage msg attrs
