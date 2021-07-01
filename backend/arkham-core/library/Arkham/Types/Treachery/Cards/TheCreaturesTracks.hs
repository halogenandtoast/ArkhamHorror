module Arkham.Types.Treachery.Cards.TheCreaturesTracks
  ( theCreaturesTracks
  , TheCreaturesTracks(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype TheCreaturesTracks = TheCreaturesTracks TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCreaturesTracks :: TreacheryCard TheCreaturesTracks
theCreaturesTracks = treachery TheCreaturesTracks Cards.theCreaturesTracks

instance HasModifiersFor env TheCreaturesTracks where
  getModifiersFor = noModifiersFor

instance HasActions env TheCreaturesTracks where
  getActions i window (TheCreaturesTracks attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env TheCreaturesTracks where
  runMessage msg t@(TheCreaturesTracks attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      broodOfYogSothothCount <- unSetAsideCount
        <$> getCount @SetAsideCount (CardCode "02255")
      if broodOfYogSothothCount == 0
        then t <$ unshiftMessages
          [ InvestigatorAssignDamage iid source DamageAny 0 2
          , Discard (toTarget attrs)
          ]
        else t <$ unshiftMessages
          [ chooseOne
            iid
            [ Label
              "Take 2 horror"
              [InvestigatorAssignDamage iid source DamageAny 0 2]
            , Label
              "Spawn a set aside Brood of Yog-Sothoth at a random location"
              [UseScenarioSpecificAbility iid Nothing 1]
            ]
          , Discard (toTarget attrs)
          ]
    _ -> TheCreaturesTracks <$> runMessage msg attrs
