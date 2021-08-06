module Arkham.Types.Asset.Cards.LiquidCourage
  ( liquidCourage
  , LiquidCourage(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Uses
import Arkham.Types.Card.CardDef
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype LiquidCourage = LiquidCourage AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liquidCourage :: AssetCard LiquidCourage
liquidCourage =
  assetWith LiquidCourage Cards.liquidCourage (startingUsesL ?~ Uses Supply 4)

instance HasActions env LiquidCourage where
  getActions iid NonFast (LiquidCourage a) = pure
    [ UseAbility
        iid
        (restrictedAbility
          (toSource a)
          1
          (InvestigatorExists
          $ InvestigatorAtYourLocation
          <> InvestigatorWithHorror
          )
          (ActionAbility Nothing
          $ Costs [ActionCost 1, UseCost (toId a) Supply 1]
          )
        )
    | ownedBy a iid
    ]
  getActions iid window (LiquidCourage attrs) = getActions iid window attrs

instance HasModifiersFor env LiquidCourage

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasSet InvestigatorId env LocationId
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env LiquidCourage where
  runMessage msg a@(LiquidCourage attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      iids <- getSetList @InvestigatorId lid
      let
        doAbilityEffect iid' =
          [ HealHorror (InvestigatorTarget iid') 1
          , BeginSkillTest
            iid'
            source
            (InvestigatorTarget iid')
            Nothing
            SkillWillpower
            2
          ]
      a <$ case iids of
        [] -> pure ()
        [iid'] -> pushAll $ doAbilityEffect iid'
        _ -> push
          (chooseOne
            iid
            [ TargetLabel (InvestigatorTarget iid') (doAbilityEffect iid')
            | iid' <- iids
            ]
          )
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a
      <$ push (HealHorror (InvestigatorTarget iid) 1)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a <$ push (RandomDiscard iid)
    _ -> LiquidCourage <$> runMessage msg attrs
