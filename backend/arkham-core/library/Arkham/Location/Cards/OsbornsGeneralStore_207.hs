module Arkham.Location.Cards.OsbornsGeneralStore_207 (
  osbornsGeneralStore_207,
  OsbornsGeneralStore_207 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card.CardType
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (osbornsGeneralStore_207)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype OsbornsGeneralStore_207 = OsbornsGeneralStore_207 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

osbornsGeneralStore_207 :: LocationCard OsbornsGeneralStore_207
osbornsGeneralStore_207 =
  location OsbornsGeneralStore_207 Cards.osbornsGeneralStore_207 3 (PerPlayer 1)

instance HasAbilities OsbornsGeneralStore_207 where
  getAbilities (OsbornsGeneralStore_207 attrs) =
    let rest = withDrawCardUnderneathAction attrs
     in [ restrictedAbility attrs 1 Here
          $ ActionAbility []
          $ Costs
            [ActionCost 1, ResourceCost 1]
        | locationRevealed attrs
        ]
          <> rest

instance RunMessage OsbornsGeneralStore_207 where
  runMessage msg l@(OsbornsGeneralStore_207 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push
        $ search
          iid
          source
          (InvestigatorTarget iid)
          [fromTopOfDeck 3]
          (CardWithType AssetType <> CardWithTrait Item)
          (DrawFound iid 1)
      pure l
    _ -> OsbornsGeneralStore_207 <$> runMessage msg attrs
