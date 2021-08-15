module Arkham.Types.Asset.Cards.DrWilliamTMaleson
  ( drWilliamTMaleson
  , DrWilliamTMaleson(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.CardType
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Window

newtype DrWilliamTMaleson = DrWilliamTMaleson AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drWilliamTMaleson :: AssetCard DrWilliamTMaleson
drWilliamTMaleson = ally DrWilliamTMaleson Cards.drWilliamTMaleson (2, 2)

ability :: AssetAttrs -> Ability
ability attrs = mkAbility
  attrs
  1
  (ResponseAbility
  $ Costs [ExhaustCost (toTarget attrs), PlaceClueOnLocationCost 1]
  )

instance HasAbilities env DrWilliamTMaleson where
  getAbilities iid (WhenDrawCard who card) (DrWilliamTMaleson attrs)
    | ownedBy attrs iid && iid == who = pure
      [ ability attrs | toCardType card `elem` encounterCardTypes ]
  getAbilities _ _ _ = pure []

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
