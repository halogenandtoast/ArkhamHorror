module Arkham.Location.Cards.DowntownFirstBankOfArkham
  ( DowntownFirstBankOfArkham(..)
  , downtownFirstBankOfArkham
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( downtownFirstBankOfArkham )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Message

newtype DowntownFirstBankOfArkham = DowntownFirstBankOfArkham LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

downtownFirstBankOfArkham :: LocationCard DowntownFirstBankOfArkham
downtownFirstBankOfArkham = location
  DowntownFirstBankOfArkham
  Cards.downtownFirstBankOfArkham
  3
  (PerPlayer 1)

instance HasAbilities DowntownFirstBankOfArkham where
  getAbilities (DowntownFirstBankOfArkham attrs) = withBaseAbilities
    attrs
    [ limitedAbility (PlayerLimit PerGame 1)
      $ restrictedAbility attrs 1 (Here <> CanGainResources)
      $ ActionAbility Nothing
      $ ActionCost 1
    | locationRevealed attrs
    ]

instance RunMessage DowntownFirstBankOfArkham where
  runMessage msg l@(DowntownFirstBankOfArkham attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push (TakeResources iid 3 False)
    _ -> DowntownFirstBankOfArkham <$> runMessage msg attrs
