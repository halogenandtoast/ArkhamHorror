module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.InstrumentCloset (instrumentCloset) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Trait (Trait (Ally, Item))

newtype InstrumentCloset = InstrumentCloset LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instrumentCloset :: LocationCard InstrumentCloset
instrumentCloset =
  locationWith InstrumentCloset Cards.instrumentCloset 3 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 1) Anywhere

instance HasModifiersFor InstrumentCloset where
  {- "While you are at Instrument Closet, treat each of your non-weakness Ally
  assets as if its text box were blank (except for Traits)." -}
  getModifiersFor (InstrumentCloset a) = do
    modifySelect
      a
      (AssetWithTrait Ally <> NonWeaknessAsset <> AssetControlledBy (investigatorAt a.id))
      [Blank]

instance HasAbilities InstrumentCloset where
  -- "[action]: Search the top 9 cards of your deck for an Item asset and draw it. (Limit once per round)"
  getAbilities (InstrumentCloset a) =
    extend1 a $ playerLimit PerRound $ restricted a 1 Here actionAbility

instance RunMessage InstrumentCloset where
  runMessage msg l@(InstrumentCloset attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search
        iid
        (attrs.ability 1)
        iid
        [fromTopOfDeck 9]
        (basic $ #asset <> CardWithTrait Item)
        (DrawFound iid 1)
      pure l
    _ -> InstrumentCloset <$> liftRunMessage msg attrs
