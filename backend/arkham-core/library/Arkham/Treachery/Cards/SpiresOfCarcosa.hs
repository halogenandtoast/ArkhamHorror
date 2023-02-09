module Arkham.Treachery.Cards.SpiresOfCarcosa
  ( spiresOfCarcosa
  , SpiresOfCarcosa(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Runner
import Arkham.Treachery.Cards qualified as Cards

newtype SpiresOfCarcosa = SpiresOfCarcosa TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiresOfCarcosa :: TreacheryCard SpiresOfCarcosa
spiresOfCarcosa = treachery SpiresOfCarcosa Cards.spiresOfCarcosa

instance HasAbilities SpiresOfCarcosa where
  getAbilities (SpiresOfCarcosa a) =
    [ restrictedAbility a 1 OnSameLocation
      $ ActionAbility (Just Action.Investigate)
      $ ActionCost 1
      ]
      <> case treacheryAttachedTarget a of
           Just (LocationTarget lid) ->
             [ restrictedAbility
                   a
                   2
                   (LocationExists $ LocationWithId lid <> LocationWithoutDoom)
                 $ SilentForcedAbility AnyWindow
             ]
           _ -> []

instance RunMessage SpiresOfCarcosa where
  runMessage msg t@(SpiresOfCarcosa attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid -> pushAll
        [ AttachTreachery (toId attrs) (LocationTarget lid)
        , PlaceDoom (LocationTarget lid) 2
        ]
      pure t
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid -> push $ Investigate
        iid
        lid
        source
        (Just $ toTarget attrs)
        SkillIntellect
        False
      pure t
    Successful (Action.Investigate, _) _ _ target _ | isTarget attrs target ->
      case treacheryAttachedTarget attrs of
        Just location -> t <$ push (RemoveDoom location 1)
        Nothing -> error "must be attached to location to trigger ability"
    UseCardAbility _ source 2 _ _ | isSource attrs source ->
      t <$ push (Discard (toAbilitySource attrs 2) $ toTarget attrs)
    _ -> SpiresOfCarcosa <$> runMessage msg attrs
