module Arkham.Types.Location.Cards.SanMarcoBasilica
  ( sanMarcoBasilica
  , SanMarcoBasilica(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype SanMarcoBasilica = SanMarcoBasilica LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanMarcoBasilica :: LocationCard SanMarcoBasilica
sanMarcoBasilica = locationWith
  SanMarcoBasilica
  Cards.sanMarcoBasilica
  3
  (Static 0)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasModifiersFor env SanMarcoBasilica

ability :: LocationAttrs -> Ability
ability a = mkAbility a 1 (ActionAbility Nothing $ ActionCost 1)

instance ActionRunner env => HasActions env SanMarcoBasilica where
  getActions iid NonFast (SanMarcoBasilica attrs) = withBaseActions
    iid
    NonFast
    attrs
    do
      pure [UseAbility iid (ability attrs)]
  getActions iid window (SanMarcoBasilica attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env SanMarcoBasilica where
  runMessage msg l@(SanMarcoBasilica attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      innocentRevelerIds <- selectList
        (AssetOwnedBy iid <> AssetIs Assets.innocentReveler)
      l <$ push
        (chooseOne
          iid
          [ TargetLabel
              (AssetTarget innocentRevelerId)
              [ PlaceUnderneath
                ActDeckTarget
                [ PlayerCard $ lookupPlayerCard
                    Assets.innocentReveler
                    (unAssetId innocentRevelerId)
                ]
              , RemoveFromGame (AssetTarget innocentRevelerId)
              ]
          | innocentRevelerId <- innocentRevelerIds
          ]
        )
    _ -> SanMarcoBasilica <$> runMessage msg attrs
