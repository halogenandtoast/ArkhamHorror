module Arkham.Types.Asset.Cards.ClarityOfMind
  ( clarityOfMind
  , ClarityOfMind(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype ClarityOfMind = ClarityOfMind AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clarityOfMind :: AssetCard ClarityOfMind
clarityOfMind = arcane ClarityOfMind Cards.clarityOfMind

instance HasActions ClarityOfMind where
  getActions (ClarityOfMind a) =
    [ restrictedAbility
        (toSource a)
        1
        (OwnsThis <> InvestigatorExists
          (InvestigatorAtYourLocation <> InvestigatorWithHorror)
        )
        (ActionAbility Nothing $ Costs [ActionCost 1, UseCost (toId a) Charge 1]
        )
    ]

instance HasModifiersFor env ClarityOfMind

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasSet InvestigatorId env LocationId
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env ClarityOfMind where
  runMessage msg a@(ClarityOfMind attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      iids <- getSetList @InvestigatorId lid
      a <$ push
        (chooseOne
          iid
          [ TargetLabel
              (InvestigatorTarget iid')
              [HealHorror (InvestigatorTarget iid') 1]
          | iid' <- iids
          ]
        )
    _ -> ClarityOfMind <$> runMessage msg attrs
