{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.DowntownArkhamAsylum
  ( DowntownArkhamAsylum(..)
  , downtownArkhamAsylum
  )
where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype DowntownArkhamAsylum = DowntownArkhamAsylum Attrs
  deriving newtype (Show, ToJSON, FromJSON)

downtownArkhamAsylum :: DowntownArkhamAsylum
downtownArkhamAsylum =
  DowntownArkhamAsylum
    $ (baseAttrs "01131" "Downtown" 4 (PerPlayer 2) Triangle [Moon, T] [Arkham])
        { locationVictory = Just 1
        }

instance HasModifiersFor env investigator DowntownArkhamAsylum where
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerGame
  }

instance (ActionRunner env investigator) => HasActions env investigator DowntownArkhamAsylum where
  getActions i NonFast (DowntownArkhamAsylum attrs@Attrs {..})
    | locationRevealed = do
      baseActions <- getActions i NonFast attrs
      unused <- getIsUnused i (ability attrs)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction (getId () i) (ability attrs)
           | unused
             && atLocation i attrs
             && hasActionsRemaining i Nothing locationTraits
           ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env DowntownArkhamAsylum where
  runMessage msg l@(DowntownArkhamAsylum attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage (HealHorror (InvestigatorTarget iid) 3)
    _ -> DowntownArkhamAsylum <$> runMessage msg attrs
