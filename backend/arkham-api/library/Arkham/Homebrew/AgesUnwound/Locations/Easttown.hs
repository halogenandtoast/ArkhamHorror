module Arkham.Homebrew.AgesUnwound.Locations.Easttown (easttown) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Ally))

newtype Easttown = Easttown LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easttown :: LocationCard Easttown
easttown = symbolLabel $ location Easttown Cards.easttown 3 (PerPlayer 1)

{- | "While you are in Easttown, reduce the cost of each [[Ally]] asset you play
by 2 and increase the cost of each other asset you play by 1."
-}
instance HasModifiersFor Easttown where
  getModifiersFor (Easttown a) =
    modifySelect
      a
      (investigatorAt a.id)
      [ ReduceCostOf (#asset <> CardWithTrait Ally) 2
      , IncreaseCostOf (basic $ #asset <> NotCard (CardWithTrait Ally)) 1
      ]

{- | "[action] Discard an asset you control: Gain 2 clues (from the token pool).
(Group limit once per round.)"
-}
instance HasAbilities Easttown where
  getAbilities (Easttown a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> youExist (HasMatchingAsset DiscardableAsset))
      $ actionAbilityWithCost (DiscardAssetCost AnyAsset)

instance RunMessage Easttown where
  runMessage msg l@(Easttown attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainClues iid (attrs.ability 1) 2
      pure l
    _ -> Easttown <$> liftRunMessage msg attrs
