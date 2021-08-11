module Arkham.Types.Asset.Cards.DrHenryArmitage
  ( DrHenryArmitage(..)
  , drHenryArmitage
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Restriction
import qualified Arkham.Types.Timing as Timing

newtype DrHenryArmitage = DrHenryArmitage AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drHenryArmitage :: AssetCard DrHenryArmitage
drHenryArmitage = ally DrHenryArmitage Cards.drHenryArmitage (2, 2)

instance HasModifiersFor env DrHenryArmitage

instance HasActions DrHenryArmitage where
  getActions (DrHenryArmitage x) =
    [ restrictedAbility
        x
        1
        OwnsThis
        (ReactionAbility (DrawCard Timing.After You (CardFromDeck YourDeck))
        $ Costs [DiscardDrawnCardCost, ExhaustThis]
        )
    ]

instance (AssetRunner env) => RunMessage env DrHenryArmitage where
  runMessage msg a@(DrHenryArmitage attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (TakeResources iid 3 False)
    _ -> DrHenryArmitage <$> runMessage msg attrs
