module Arkham.Types.Asset.Cards.StrangeSolution
  ( strangeSolution
  , StrangeSolution(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.CampaignLogKey
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype StrangeSolution = StrangeSolution AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolution :: AssetCard StrangeSolution
strangeSolution = asset StrangeSolution Cards.strangeSolution

instance HasActions env StrangeSolution where
  getActions iid NonFast (StrangeSolution attrs) | ownedBy attrs iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    ]
  getActions iid window (StrangeSolution attrs) = getActions iid window attrs

instance HasModifiersFor env StrangeSolution where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env StrangeSolution where
  runMessage msg a@(StrangeSolution attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage
        (BeginSkillTest
          iid
          source
          (InvestigatorTarget iid)
          Nothing
          SkillIntellect
          4
        )
    PassedSkillTest iid _ source _ _ _ | isSource attrs source ->
      a <$ unshiftMessages
        [ Discard (toTarget attrs)
        , DrawCards iid 2 False
        , Record YouHaveIdentifiedTheSolution
        ]
    _ -> StrangeSolution <$> runMessage msg attrs
