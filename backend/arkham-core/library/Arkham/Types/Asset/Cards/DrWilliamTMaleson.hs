module Arkham.Types.Asset.Cards.DrWilliamTMaleson
  ( drWilliamTMaleson
  , DrWilliamTMaleson(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (When)
import Arkham.Types.Restriction
import Arkham.Types.Timing

newtype DrWilliamTMaleson = DrWilliamTMaleson AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drWilliamTMaleson :: AssetCard DrWilliamTMaleson
drWilliamTMaleson = ally DrWilliamTMaleson Cards.drWilliamTMaleson (2, 2)

instance HasActions DrWilliamTMaleson where
  getActions (DrWilliamTMaleson attrs) =
    [ restrictedAbility
        attrs
        1
        OwnsThis
        (ReactionAbility (DrawCard When You $ BasicCardMatch IsEncounterCard)
        $ Costs [ExhaustCost (toTarget attrs), PlaceClueOnLocationCost 1]
        )
    ]

instance HasModifiersFor env DrWilliamTMaleson

dropUntilDraw :: [Message] -> [Message]
dropUntilDraw = dropWhile (notElem DrawEncounterCardMessage . messageType)

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env DrWilliamTMaleson where
  runMessage msg a@(DrWilliamTMaleson attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      card <- withQueue $ \queue ->
        let
          InvestigatorDrewEncounterCard _ card' : queue' = dropUntilDraw queue
        in (queue', card')
      a <$ pushAll
        [ShuffleIntoEncounterDeck [card], InvestigatorDrawEncounterCard iid]
    _ -> DrWilliamTMaleson <$> runMessage msg attrs
