module Arkham.Treachery.Cards.ThePitBelow
  ( thePitBelow
  , ThePitBelow(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype ThePitBelow = ThePitBelow TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePitBelow :: TreacheryCard ThePitBelow
thePitBelow = treachery ThePitBelow Cards.thePitBelow

instance HasModifiersFor ThePitBelow where
  getModifiersFor (LocationTarget lid) (ThePitBelow attrs) =
    pure
      $ toModifiers attrs [ ShroudModifier 1 | treacheryOnLocation lid attrs ]
  getModifiersFor _ _ = pure []

instance HasAbilities ThePitBelow where
  getAbilities (ThePitBelow a) =
    [mkAbility a 1 $ ForcedAbility $ RoundEnds Timing.When]

instance RunMessage ThePitBelow where
  runMessage msg t@(ThePitBelow attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      case mlid of
        Nothing -> push (Discard (toSource attrs) $ toTarget attrs)
        Just lid -> do
          hasThePitBelow <-
            selectAny
            $ TreacheryAt (LocationWithId lid)
            <> treacheryIs Cards.thePitBelow
          if hasThePitBelow
            then pushAll [Discard (toSource attrs) (toTarget attrs), Surge iid (toSource attrs)]
            else push (AttachTreachery (toId attrs) $ LocationTarget lid)
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      iids <-
        selectList $ InvestigatorAt $ LocationWithTreachery $ TreacheryWithId
          (toId attrs)
      pushAll
        $ [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 3 0
          | iid <- iids
          ]
        <> [Discard (toAbilitySource attrs 1) $ toTarget attrs]
      pure t
    _ -> ThePitBelow <$> runMessage msg attrs
