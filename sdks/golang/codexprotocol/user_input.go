package codexprotocol

import "encoding/json"

// UserInputType discriminates between user input kinds.
type UserInputType string

const (
	UserInputTypeText       UserInputType = "text"
	UserInputTypeImage      UserInputType = "image"
	UserInputTypeLocalImage UserInputType = "local_image"
	UserInputTypeSkill      UserInputType = "skill"
)

// UserInput represents user input in various forms.
// Source: codex-rs/protocol/src/user_input.rs
type UserInput interface {
	userInputType() UserInputType
}

// TextInput represents text user input.
type TextInput struct {
	Type UserInputType `json:"type"`
	Text string        `json:"text"`
}

func (t TextInput) userInputType() UserInputType { return UserInputTypeText }

// ImageInput represents a pre-encoded data URL image.
type ImageInput struct {
	Type     UserInputType `json:"type"`
	ImageURL string        `json:"image_url"`
}

func (i ImageInput) userInputType() UserInputType { return UserInputTypeImage }

// LocalImageInput represents a local image path (converted to base64 during serialization).
type LocalImageInput struct {
	Type UserInputType `json:"type"`
	Path string        `json:"path"`
}

func (l LocalImageInput) userInputType() UserInputType { return UserInputTypeLocalImage }

// SkillInput represents a skill selection.
type SkillInput struct {
	Type UserInputType `json:"type"`
	Name string        `json:"name"`
	Path string        `json:"path"`
}

func (s SkillInput) userInputType() UserInputType { return UserInputTypeSkill }

// NewTextInput creates a new text user input.
func NewTextInput(text string) TextInput {
	return TextInput{
		Type: UserInputTypeText,
		Text: text,
	}
}

// NewImageInput creates a new image user input with a data URL.
func NewImageInput(imageURL string) ImageInput {
	return ImageInput{
		Type:     UserInputTypeImage,
		ImageURL: imageURL,
	}
}

// NewLocalImageInput creates a new local image user input.
func NewLocalImageInput(path string) LocalImageInput {
	return LocalImageInput{
		Type: UserInputTypeLocalImage,
		Path: path,
	}
}

// NewSkillInput creates a new skill user input.
func NewSkillInput(name, path string) SkillInput {
	return SkillInput{
		Type: UserInputTypeSkill,
		Name: name,
		Path: path,
	}
}

// ParseUserInput parses a JSON user input into the appropriate type.
func ParseUserInput(data []byte) (UserInput, error) {
	var raw struct {
		Type UserInputType `json:"type"`
	}
	if err := json.Unmarshal(data, &raw); err != nil {
		return nil, err
	}

	switch raw.Type {
	case UserInputTypeText:
		var input TextInput
		if err := json.Unmarshal(data, &input); err != nil {
			return nil, err
		}
		return input, nil
	case UserInputTypeImage:
		var input ImageInput
		if err := json.Unmarshal(data, &input); err != nil {
			return nil, err
		}
		return input, nil
	case UserInputTypeLocalImage:
		var input LocalImageInput
		if err := json.Unmarshal(data, &input); err != nil {
			return nil, err
		}
		return input, nil
	case UserInputTypeSkill:
		var input SkillInput
		if err := json.Unmarshal(data, &input); err != nil {
			return nil, err
		}
		return input, nil
	default:
		// Return as text input with raw data
		var input TextInput
		if err := json.Unmarshal(data, &input); err != nil {
			return nil, err
		}
		return input, nil
	}
}
