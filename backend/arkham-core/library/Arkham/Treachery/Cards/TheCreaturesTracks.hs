module Arkham.Treachery.Cards.TheCreaturesTracks
  ( theCreaturesTracks
  , TheCreaturesTracks(..)
  ) where

import Arkham.Prelude

import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Message
import Arkham.Query
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype TheCreaturesTracks = TheCreaturesTracks TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCreaturesTracks :: TreacheryCard TheCreaturesTracks
theCreaturesTracks = treachery TheCreaturesTracks Cards.theCreaturesTracks

instance TreacheryRunner env => RunMessage env TheCreaturesTracks where
  runMessage msg t@(TheCreaturesTracks attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      broodOfYogSothothCount <- unSetAsideCount
        <$> getCount @SetAsideCount (CardCode "02255")
      if broodOfYogSothothCount == 0
        then t <$ push (InvestigatorAssignDamage iid source DamageAny 0 2)
        else t <$ push
          (chooseOne
            iid
            [ Label
              "Take 2 horror"
              [InvestigatorAssignDamage iid source DamageAny 0 2]
            , Label
              "Spawn a set aside Brood of Yog-Sothoth at a random location"
              [ChooseRandomLocation (toTarget attrs) mempty]
            ]
          )
    ChosenRandomLocation target lid | isTarget attrs target -> do
      setAsideBroodOfYogSothoth <- shuffleM =<< getSetAsideBroodOfYogSothoth
      case setAsideBroodOfYogSothoth of
        [] -> pure t
        (x : _) -> t <$ push (CreateEnemyAt x lid Nothing)
    _ -> TheCreaturesTracks <$> runMessage msg attrs
