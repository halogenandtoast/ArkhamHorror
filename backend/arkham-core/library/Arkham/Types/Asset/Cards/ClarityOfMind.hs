module Arkham.Types.Asset.Cards.ClarityOfMind
  ( clarityOfMind
  , ClarityOfMind(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype ClarityOfMind = ClarityOfMind AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clarityOfMind :: AssetCard ClarityOfMind
clarityOfMind = arcane ClarityOfMind Cards.clarityOfMind

instance HasActions env ClarityOfMind where
  getActions iid NonFast (ClarityOfMind a) = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility
          (toSource a)
          1
          (ActionAbility Nothing
          $ Costs [ActionCost 1, UseCost (toId a) Charge 1]
          )
        )
    | ownedBy a iid
    ]
  getActions _ _ _ = pure []

instance HasModifiersFor env ClarityOfMind where
  getModifiersFor = noModifiersFor

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasSet InvestigatorId env LocationId
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env ClarityOfMind where
  runMessage msg a@(ClarityOfMind attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      ClarityOfMind <$> runMessage msg (attrs & usesL .~ Uses Charge 3)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      iids <- getSetList @InvestigatorId lid
      a <$ unshiftMessage
        (chooseOne
          iid
          [ TargetLabel
              (InvestigatorTarget iid')
              [HealHorror (InvestigatorTarget iid') 1]
          | iid' <- iids
          ]
        )
    _ -> ClarityOfMind <$> runMessage msg attrs
