module Arkham.Types.Treachery.Cards.SpiresOfCarcosa
  ( spiresOfCarcosa
  , SpiresOfCarcosa(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype SpiresOfCarcosa = SpiresOfCarcosa TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env)
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

instance TreacheryRunner env => RunMessage env SpiresOfCarcosa where
  runMessage msg t@(SpiresOfCarcosa attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId iid
      t <$ pushAll
        [ AttachTreachery (toId attrs) (LocationTarget lid)
        , PlaceDoom (LocationTarget lid) 2
        ]
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId iid
      t
        <$ push
             (Investigate
               iid
               lid
               source
               (Just $ toTarget attrs)
               SkillIntellect
               False
             )
    Successful (Action.Investigate, _) _ _ target | isTarget attrs target -> do
      case treacheryAttachedTarget attrs of
        Just location -> t <$ push (RemoveDoom location 1)
        Nothing -> error "must be attached to location to trigger ability"
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> SpiresOfCarcosa <$> runMessage msg attrs
