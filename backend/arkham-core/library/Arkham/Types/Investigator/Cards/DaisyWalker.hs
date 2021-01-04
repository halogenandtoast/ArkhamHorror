{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.DaisyWalker
  ( DaisyWalker(..)
  , daisyWalker
  )
where

import Arkham.Import

import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype DaisyWalker = DaisyWalker Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env DaisyWalker where
  getModifiersFor source target (DaisyWalker attrs) =
    getModifiersFor source target attrs

daisyWalker :: DaisyWalker
daisyWalker =
  DaisyWalker $ (baseAttrs "01002" "Daisy Walker" Seeker stats [Miskatonic])
    { investigatorTomeActions = Just 1
    }
 where
  stats = Stats
    { health = 5
    , sanity = 9
    , willpower = 3
    , intellect = 5
    , combat = 2
    , agility = 2
    }

instance ActionRunner env => HasActions env DaisyWalker where
  getActions i window (DaisyWalker attrs) = getActions i window attrs

instance HasTokenValue env DaisyWalker where
  getTokenValue (DaisyWalker attrs) iid ElderSign
    | iid == investigatorId attrs = pure
    $ TokenValue ElderSign (PositiveModifier 0)
  getTokenValue (DaisyWalker attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => RunMessage env DaisyWalker where
  runMessage msg i@(DaisyWalker attrs@Attrs {..}) = case msg of
    ResetGame -> do
      attrs' <- runMessage msg attrs
      pure $ DaisyWalker $ attrs' { investigatorTomeActions = Just 1 }
    ActivateCardAbilityAction iid ability@Ability {..}
      | iid == investigatorId && sourceIsAsset abilitySource -> do
        let (AssetSource aid) = abilitySource
        traits <- getSet aid
        if Tome
          `elem` traits
          && fromJustNote "Must be set" investigatorTomeActions
          > 0
        then
          case abilityType of
            ForcedAbility -> DaisyWalker <$> runMessage msg attrs
            FastAbility _ -> DaisyWalker <$> runMessage msg attrs
            ReactionAbility _ -> DaisyWalker <$> runMessage msg attrs
            ActionAbility mAction cost -> if totalActionCost cost > 0
              then DaisyWalker <$> runMessage
                (ActivateCardAbilityAction
                  iid
                  (ability
                    { abilityType = ActionAbility
                      mAction
                      (decreaseActionCost cost 1)
                    }
                  )
                )
                (attrs
                  { investigatorTomeActions =
                    max 0 . subtract 1 <$> investigatorTomeActions
                  }
                )
              else DaisyWalker <$> runMessage msg attrs
        else
          DaisyWalker <$> runMessage msg attrs
    PassedSkillTest iid _ _ (DrawnTokenTarget token) _
      | iid == investigatorId -> case drawnTokenFace token of
        ElderSign -> do
          tomeCount <- unAssetCount <$> getCount (iid, [Tome])
          i <$ when
            (tomeCount > 0)
            (unshiftMessage $ Ask
              iid
              (ChooseOne
                [ DrawCards iid tomeCount False
                , Continue "Do not use Daisy's ability"
                ]
              )
            )
        _ -> pure i
    BeginRound -> DaisyWalker
      <$> runMessage msg (attrs { investigatorTomeActions = Just 1 })
    _ -> DaisyWalker <$> runMessage msg attrs
