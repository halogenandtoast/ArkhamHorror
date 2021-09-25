module Arkham.Types.Treachery.Cards.SpiresOfCarcosa
  ( spiresOfCarcosa
  , SpiresOfCarcosa(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Message
import Arkham.Types.Query
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

instance TreacheryRunner env => RunMessage env SpiresOfCarcosa where
  runMessage msg t@(SpiresOfCarcosa attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId iid
      t <$ pushAll
        [ AttachTreachery (toId attrs) (LocationTarget lid)
        , PlaceDoom (LocationTarget lid) 2
        ]
    RemoveDoom target@(LocationTarget lid) n
      | Just target == treacheryAttachedTarget attrs -> do
        doomCount <- unDoomCount <$> getCount lid
        t <$ when (doomCount - n <= 0) (push $ Discard (toTarget attrs))
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
    _ -> SpiresOfCarcosa <$> runMessage msg attrs
