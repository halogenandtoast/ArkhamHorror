module Arkham.Homebrew.CircusExMortis.Locations.RemoteCabin (remoteCabin) where

import Arkham.Cost
import Arkham.Helpers.Modifiers
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (neighbouringMoonlitForestColumn)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token (Token (..), countTokens)
import Arkham.Trait (Trait (Woods))

newtype RemoteCabin = RemoteCabin LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

remoteCabin :: LocationCard RemoteCabin
remoteCabin =
  location RemoteCabin Cards.remoteCabin 4 (Static 1)

instance HasModifiersFor RemoteCabin where
  getModifiersFor (RemoteCabin a) = do
    let damage = countTokens Damage a.tokens
    modifySelectWith a Anyone setActiveDuringSetup [CannotEnter a.id | damage < 2]

    let forests = neighbouringMoonlitForestColumn (be a)
    modifySelfWith a setActiveDuringSetup [ConnectedToWhen (be a) forests]
    modifySelectWith a forests setActiveDuringSetup [ConnectedToWhen forests (be a)]

    -- "As an additional cost to move from Remote Cabin to a non-Woods location, place 1
    -- doom on a card you control." The only cards you control that can hold doom are
    -- assets, so the choice is over those.
    modifySelect
      a
      (InvestigatorAt $ be a)
      [ AdditionalCostToEnterMatching (not_ $ LocationWithTrait Woods)
          $ SourcedCost (toSource a)
          $ AssetDoomCost 1 (AssetControlledBy You)
      ]

instance RunMessage RemoteCabin where
  runMessage msg (RemoteCabin attrs) = runQueueT $ case msg of
    _ -> RemoteCabin <$> liftRunMessage msg attrs
