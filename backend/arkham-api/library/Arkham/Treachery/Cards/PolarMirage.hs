module Arkham.Treachery.Cards.PolarMirage (polarMirage, PolarMirage (..)) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (DiscoverClues)

newtype PolarMirage = PolarMirage TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

polarMirage :: TreacheryCard PolarMirage
polarMirage = treachery PolarMirage Cards.polarMirage

-- TODO: Need to update for Frenzied Explorer and Lost Researcher
instance HasAbilities PolarMirage where
  getAbilities (PolarMirage a) =
    [ mkAbility a 1
        $ forced
        $ oneOf
          [ DiscoverClues #after You (locationWithTreachery a) AnyValue
          , TakeControlOfClues #after You
              $ oneOf
                [ SourceIsLocation (locationWithTreachery a)
                , SourceIsEnemy (EnemyAt $ locationWithTreachery a)
                ]
          ]
    ]

instance RunMessage PolarMirage where
  runMessage msg t@(PolarMirage attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ls <-
        select
          $ NearestLocationTo iid
          $ LocationWithoutTreachery (treacheryIs Cards.kindredMist)
          <> LocationWithAnyClues
      chooseTargetM iid ls $ place attrs . AttachedToLocation
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardAll iid (attrs.ability 1) $ NonWeakness <> DiscardableCard
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> PolarMirage <$> liftRunMessage msg attrs
