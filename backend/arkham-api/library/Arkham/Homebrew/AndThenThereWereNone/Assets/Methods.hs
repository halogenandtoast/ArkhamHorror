{- | The fifteen [[Method]] cards.

A Method prints a name, a single skill icon and a line of flavor -- "The victim
was shot with an old army revolver at a very close distance." -- and nothing
else. All the rules that move them live elsewhere: the locations reveal them
from underneath, act 1 attaches them to [[Victim]] assets, and act 1b tallies
their icons to name the culprit. So the fifteen share one entity the way
@Arkham.Homebrew.ConsternationOnTheConstellation.Assets.CrateOfGoods@ does, and
only the card def differs.
-}
module Arkham.Homebrew.AndThenThereWereNone.Assets.Methods (
  blowpipe,
  chloralHydrateOverdose,
  crushedByDebris,
  cyanideInjection,
  drownedInTheSea,
  electrocution,
  fellFromAHeight,
  hatchet,
  improvisedItem,
  knife,
  poisonedDrink,
  ropeNoose,
  serviceRevolver,
  smotheredInSleep,
  straightRazor,
) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Cards

newtype Method = Method AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blowpipe :: AssetCard Method
blowpipe = asset Method Cards.blowpipe

chloralHydrateOverdose :: AssetCard Method
chloralHydrateOverdose = asset Method Cards.chloralHydrateOverdose

crushedByDebris :: AssetCard Method
crushedByDebris = asset Method Cards.crushedByDebris

cyanideInjection :: AssetCard Method
cyanideInjection = asset Method Cards.cyanideInjection

drownedInTheSea :: AssetCard Method
drownedInTheSea = asset Method Cards.drownedInTheSea

electrocution :: AssetCard Method
electrocution = asset Method Cards.electrocution

fellFromAHeight :: AssetCard Method
fellFromAHeight = asset Method Cards.fellFromAHeight

hatchet :: AssetCard Method
hatchet = asset Method Cards.hatchet

improvisedItem :: AssetCard Method
improvisedItem = asset Method Cards.improvisedItem

knife :: AssetCard Method
knife = asset Method Cards.knife

poisonedDrink :: AssetCard Method
poisonedDrink = asset Method Cards.poisonedDrink

ropeNoose :: AssetCard Method
ropeNoose = asset Method Cards.ropeNoose

serviceRevolver :: AssetCard Method
serviceRevolver = asset Method Cards.serviceRevolver

smotheredInSleep :: AssetCard Method
smotheredInSleep = asset Method Cards.smotheredInSleep

straightRazor :: AssetCard Method
straightRazor = asset Method Cards.straightRazor

instance RunMessage Method where
  runMessage msg (Method attrs) = runQueueT $ Method <$> liftRunMessage msg attrs
